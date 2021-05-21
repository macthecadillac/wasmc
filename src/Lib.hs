{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Applicative ((<|>), Alternative)
import Control.Monad.Except
import qualified Control.Monad.Fail as F
import Control.Monad.Reader
    ( runReader, MonadReader(ask), Reader, ReaderT(ReaderT) )
import Control.Monad.State.Lazy
import Data.Bifunctor
import qualified Data.ByteString.Lazy as B
import Data.Functor
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Data.Word
import Data.Tuple
import Debug.Trace

import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Structure as S
import qualified Language.Wasm.Structure (Function(..))

import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as Conv
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Float as Float
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Type as Type

import Numeric.Natural
import Utils (appendIfLast, makeName, splitAfter, unsnoc)

-- a record for per-function state variables
data FuncST = FuncST { operandStack :: [AST.Operand]
                     , localIdentifier :: Natural
                     , blockIdentifier :: Natural
                     , blockScopeStack :: [Name.Name]
                     , renameMap :: M.Map Name.Name Name.Name
                     , localConst :: M.Map Name.Name (M.Map Name.Name Constant.Constant)  -- the constants associated with the identifier of the incoming block
                     , localVariableTypes :: M.Map Name.Name Type.Type }
                     deriving (Show)

data FunctionType = FT { arguments :: [Type.Type], returnType :: Type.Type }
  deriving (Show)

-- a record for per-module constants
data ModEnv = ModEnv { startFunctionIndex :: Maybe Natural
                     , functionTypes :: M.Map Natural FunctionType
                    --  , globalVariableTypes :: M.Map Natural Type.Type
                      }
                     deriving (Show)

newtype ModGen a = ModGen { runModGen :: ExceptT String (Reader ModEnv) a }
  deriving (Functor, Applicative, MonadPlus, Alternative, Monad, MonadReader ModEnv, MonadError String)

newtype FuncGen a = FuncGen { runFuncGen :: StateT FuncST ModGen a }
  deriving (Functor, Applicative, MonadPlus, Alternative, Monad, MonadReader ModEnv, F.MonadFail, MonadError String, MonadState FuncST)

instance F.MonadFail ModGen where
  fail = liftEither . Left

-- helper functions. Not entirely following Haskell conventions here.
evalModGen :: ModGen a -> ModEnv -> Either String a
evalModGen a = runReader (runExceptT (runModGen a))

evalFuncGen :: FuncGen a -> FuncST -> ModGen a
evalFuncGen a = evalStateT (runFuncGen a)

type BOI    = Bool -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BOOI   = Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BBOOI  = Bool -> Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type FOOI   = AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type POOI p = p -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type OOI    = AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type MOI    = Maybe AST.Operand -> AST.InstructionMetadata -> AST.Terminator

type UnOpBuilder a = a -> AST.Operand -> AST.Instruction
type BinOpBuilder a = a -> AST.Operand -> AST.Operand -> AST.Instruction
type CmpBuilder p = POOI p -> p -> AST.Operand -> AST.Operand -> AST.Instruction

boi :: UnOpBuilder BOI
boi op a = op False a []

booi :: BinOpBuilder BOOI
booi op a b = op False a b []

bbooi :: BinOpBuilder BBOOI
bbooi op a b = op False False a b []

fooi :: BinOpBuilder FOOI
fooi op a b = op AST.noFastMathFlags a b []

ooi :: BinOpBuilder OOI
ooi op a b = op a b []

pooi :: CmpBuilder p
pooi p pred a b = p pred a b []

data LLVMInstr = I (AST.Named AST.Instruction)
               | T (AST.Named AST.Terminator)
               deriving (Show)

isTerm :: LLVMInstr -> Bool
isTerm (T _) = True
isTerm _     = False

compileConst :: Constant.Constant -> FuncGen [LLVMInstr]
compileConst const = (pushOperand $ AST.ConstantOperand const) $> []

compileBinOp :: BinOpBuilder a -> a -> Type.Type -> FuncGen [LLVMInstr]
compileBinOp builder op t = do
  b          <- popOperand
  a          <- popOperand
  -- generate a new identifier for the intermediate result. In LLVM IR
  -- this amounts to saving the results to a 'variable.'
  constructor <- newInstructionConstructor t
  pure [I $ constructor $ builder op a b]

compileUnOp :: UnOpBuilder a -> a -> Type.Type -> FuncGen [LLVMInstr]
compileUnOp builder op t = do
  a          <- popOperand
  constructor <- newInstructionConstructor t
  pure [I $ constructor $ builder op a]

compileCmpOp :: POOI p -> p -> Word32 -> FuncGen [LLVMInstr]
compileCmpOp cmp pred bs = do
  b          <- popOperand
  a          <- popOperand
  constructor <- newInstructionConstructor $ Type.IntegerType bs
  pure [I $ constructor $ pooi cmp pred a b]

iBitSize :: S.BitSize -> Word32
iBitSize S.BS32 = 32
iBitSize S.BS64 = 64

fBitSize :: S.BitSize -> Type.FloatingPointType
fBitSize S.BS32 = Type.FloatFP
fBitSize S.BS64 = Type.DoubleFP

compilefMinMax :: S.BitSize -> S.FRelOp -> FuncGen [LLVMInstr]
compilefMinMax bs pred = do
  b       <- popOperand
  a       <- popOperand
  pushOperand a
  pushOperand b
  pushOperand a
  pushOperand b
  operand <- compileInstr (S.FRelOp bs pred)
  instr   <- compileInstr S.Select
  pure $ operand ++ instr

compileInstr :: S.Instruction Natural -> FuncGen [LLVMInstr]
compileInstr S.Unreachable          = undefined
compileInstr S.Nop                  = pure []

compileInstr (S.IUnOp bs S.IClz)    = undefined
compileInstr (S.IUnOp bs S.ICtz)    = undefined
compileInstr (S.IUnOp bs S.IPopcnt) = undefined

compileInstr (S.FUnOp bs S.FAbs)     = undefined
compileInstr (S.FUnOp S.BS32 S.FNeg) = compileInstr (S.F32Const (-1)) *> compileInstr (S.FBinOp S.BS32 S.FMul)
compileInstr (S.FUnOp S.BS64 S.FNeg) = compileInstr (S.F64Const (-1)) *> compileInstr (S.FBinOp S.BS64 S.FMul)
compileInstr (S.FUnOp bs S.FCeil)    = undefined
compileInstr (S.FUnOp bs S.FFloor)   = undefined
compileInstr (S.FUnOp bs S.FTrunc)   = undefined
compileInstr (S.FUnOp bs S.FNearest) = undefined
compileInstr (S.FUnOp bs S.FSqrt)    = undefined

compileInstr S.I32Eqz                = compileInstr (S.I32Const 0) *> compileInstr (S.IRelOp S.BS32 S.IEq)
compileInstr S.I64Eqz                = compileInstr (S.I64Const 0) *> compileInstr (S.IRelOp S.BS64 S.IEq)

compileInstr S.I32WrapI64            = undefined
compileInstr S.I64ExtendSI32         = undefined
compileInstr S.I64ExtendUI32         = undefined
compileInstr (S.ITruncFU bs1 bs2)    = undefined
compileInstr (S.ITruncFS bs1 bs2)    = undefined
compileInstr (S.FConvertIU bs1 bs2)  = undefined
compileInstr (S.FConvertIS bs1 bs2)  = undefined
compileInstr S.F32DemoteF64          = undefined
compileInstr S.F64PromoteF32         = undefined
compileInstr (S.IReinterpretF bs)    = undefined
compileInstr (S.FReinterpretI bs)    = undefined

compileInstr (S.IBinOp bs S.IAdd)  = compileBinOp bbooi AST.Add  $ Type.IntegerType $ iBitSize bs
compileInstr (S.IBinOp bs S.ISub)  = compileBinOp bbooi AST.Sub  $ Type.IntegerType $ iBitSize bs
compileInstr (S.IBinOp bs S.IMul)  = compileBinOp bbooi AST.Mul  $ Type.IntegerType $ iBitSize bs
compileInstr (S.IBinOp bs S.IDivS) = compileBinOp booi  AST.SDiv $ Type.IntegerType $ iBitSize bs
compileInstr (S.IBinOp bs S.IRemS) = compileBinOp ooi   AST.SRem $ Type.IntegerType $ iBitSize bs
compileInstr (S.IBinOp bs S.IAnd)  = compileBinOp ooi   AST.And  $ Type.IntegerType $ iBitSize bs
compileInstr (S.IBinOp bs S.IOr)   = compileBinOp ooi   AST.Or   $ Type.IntegerType $ iBitSize bs
compileInstr (S.IBinOp bs S.IXor)  = compileBinOp ooi   AST.Xor  $ Type.IntegerType $ iBitSize bs
compileInstr (S.IBinOp bs S.IShl)  = compileBinOp bbooi AST.Shl  $ Type.IntegerType $ iBitSize bs

compileInstr (S.IRelOp bs S.IEq)   = compileCmpOp AST.ICmp IPred.EQ  $ iBitSize bs
compileInstr (S.IRelOp bs S.INe)   = compileCmpOp AST.ICmp IPred.NE  $ iBitSize bs
compileInstr (S.IRelOp bs S.ILtU)  = compileCmpOp AST.ICmp IPred.ULT $ iBitSize bs
compileInstr (S.IRelOp bs S.ILtS)  = compileCmpOp AST.ICmp IPred.SLT $ iBitSize bs
compileInstr (S.IRelOp bs S.IGtU)  = compileCmpOp AST.ICmp IPred.UGT $ iBitSize bs
compileInstr (S.IRelOp bs S.IGtS)  = compileCmpOp AST.ICmp IPred.SGT $ iBitSize bs
compileInstr (S.IRelOp bs S.ILeU)  = compileCmpOp AST.ICmp IPred.ULE $ iBitSize bs
compileInstr (S.IRelOp bs S.ILeS)  = compileCmpOp AST.ICmp IPred.SLE $ iBitSize bs
compileInstr (S.IRelOp bs S.IGeU)  = compileCmpOp AST.ICmp IPred.UGE $ iBitSize bs
compileInstr (S.IRelOp bs S.IGeS)  = compileCmpOp AST.ICmp IPred.SGE $ iBitSize bs

compileInstr (S.FBinOp bs S.FAdd)  = compileBinOp fooi AST.FAdd $ Type.FloatingPointType $ fBitSize bs
compileInstr (S.FBinOp bs S.FSub)  = compileBinOp fooi AST.FSub $ Type.FloatingPointType $ fBitSize bs
compileInstr (S.FBinOp bs S.FMul)  = compileBinOp fooi AST.FMul $ Type.FloatingPointType $ fBitSize bs
compileInstr (S.FBinOp bs S.FDiv)  = compileBinOp fooi AST.FDiv $ Type.FloatingPointType $ fBitSize bs
compileInstr (S.FBinOp bs S.FMin)  = compilefMinMax bs S.FLt
compileInstr (S.FBinOp bs S.FMax)  = compilefMinMax bs S.FGt
-- value of the first argument, sign of the second argument
-- compileInstr (S.FBinOp bs S.FCopySign) = compileBinOp booi  AST.SDiv $ Type.IntegerType $ iBitSize bs

compileInstr (S.FRelOp bs S.FEq)   = compileCmpOp AST.FCmp FPred.OEQ $ iBitSize bs
compileInstr (S.FRelOp bs S.FNe)   = compileCmpOp AST.FCmp FPred.ONE $ iBitSize bs
compileInstr (S.FRelOp bs S.FLt)   = compileCmpOp AST.FCmp FPred.OLT $ iBitSize bs
compileInstr (S.FRelOp bs S.FGt)   = compileCmpOp AST.FCmp FPred.OGT $ iBitSize bs
compileInstr (S.FRelOp bs S.FLe)   = compileCmpOp AST.FCmp FPred.OLE $ iBitSize bs
compileInstr (S.FRelOp bs S.FGe)   = compileCmpOp AST.FCmp FPred.OGE $ iBitSize bs

compileInstr S.Select              = do
  cond       <- popOperand
  false      <- popOperand
  true       <- popOperand
  constructor <- newInstructionConstructor $ operandType true
  pure [I $ constructor $ AST.Select cond true false []]

compileInstr (S.I32Const n)        = compileConst $ Constant.Int 32 $ fromIntegral n
compileInstr (S.I64Const n)        = compileConst $ Constant.Int 64 $ fromIntegral n
compileInstr (S.F32Const n)        = compileConst $ Constant.Float $ Float.Single n
compileInstr (S.F64Const n)        = compileConst $ Constant.Float $ Float.Double n

compileInstr (S.SetGlobal n)       = error "not implemented: SetGlobal"
compileInstr (S.TeeLocal n)        = error "not implemented: TeeLocal"
compileInstr (S.SetLocal n)        = do
  -- if there is a constant associated to the variable, remove it
  let ident = makeName "local" n
  unassignConstant ident
  a <- popOperand
  case a of
    -- for references, this is acieved by renaming identifiers at the
    -- end
    (AST.LocalReference _ name) -> addRenameAction name ident $> []
    (AST.ConstantOperand const) -> assignConstant ident const $> []
    _                           -> error "unsupported operand"

compileInstr (S.GetGlobal n)       = error "not implemented: GetGlobal"

-- `<|>` is the choice operator. It tries the first branch, and if it fails,
-- goes on to try the second branch.
compileInstr (S.GetLocal n)        = const <|> ref
    where
      name = makeName "local" n
      withMsg = maybe (F.fail $ "unbound reference: " ++ show name) pure
      ref = do
        FuncST { localVariableTypes } <- get
        operand <- withMsg $ AST.LocalReference <$> M.lookup name localVariableTypes <*> pure name
        pushOperand operand
        pure []
      const = do
        FuncST { localVariableTypes, localConst, blockIdentifier } <- get
        varType     <- withMsg $ M.lookup name localVariableTypes
        constants   <- withMsg $ M.lookup name localConst
        constructor <- newInstructionConstructor varType
        let outofblockInstr = do
                pure $ pure
                     $ I
                     $ constructor
                     $ AST.Phi varType (first AST.ConstantOperand . swap <$> M.assocs constants) []
            inblockInstr c  = pushOperand (AST.ConstantOperand c) $> []
            blockID         = makeName "block" blockIdentifier
        maybe outofblockInstr inblockInstr $ M.lookup blockID constants

compileInstr (S.Call i) = do
  ModEnv { functionTypes } <- ask
  let FT { arguments, returnType } = functionTypes M.! i
      nArgs                        = L.length $ arguments
      name                         = makeName "func" i
      function                     = Right
                                   $ AST.ConstantOperand
                                   $ flip Constant.GlobalReference name 
                                   $ Type.FunctionType returnType arguments False
  -- pop the required number of operands off the `operandStack` and
  -- collect the results into a list. `replicateM` deals with the
  -- FuncGen monad.
  args       <- replicateM nArgs popOperand
  let arguments = [(operand, []) | operand <- args]
      instr     = AST.Call Nothing Conv.C [] function arguments [] []
  constructor <- newInstructionConstructor returnType
  pure [I $ constructor instr]

-- TODO: will need to be modified for multiple returns. Think about the
-- difference branches and residual items on the stack before entering each
-- branch
compileInstr S.Return = pure . T <$> returnOperandStackItems

compileInstr (S.If _ b1 b2) = do
  blockIndx <- blockIdentifier <$> get
  modify (\st -> st { blockIdentifier = blockIndx + 1 })
  let b1SubBlocks = countBlocks b1
      b2SubBlocks = countBlocks b2
      b1Ident     = makeName "block" $ blockIndx + 1
      b2Ident     = makeName "block" $ blockIndx + b1SubBlocks + 1
      fallthrough = makeName "block" (blockIndx + b1SubBlocks + b2SubBlocks)
      term        = T $ AST.Do $ AST.Br fallthrough []
      appendTerm  = appendIfLast (not . isTerm) term
  operand    <- popOperand
  b1Compiled <- concat <$> traverse compileInstr b1
  b2Compiled <- concat <$> traverse compileInstr b2
  let instr = T $ AST.Do $ AST.CondBr operand b1Ident b2Ident []
  pure $ instr : appendTerm b1Compiled ++ appendTerm b2Compiled

-- the index here is the levels of scopes. It's a WASM peculiarity
compileInstr (S.Br i)       = do
  dest <- readScope $ fromIntegral i
  pure [T $ AST.Do $ AST.Br dest []]

compileInstr (S.BrIf i)     = do
  blockIndx <- blockIdentifier <$> get
  dest      <- readScope $ fromIntegral i
  operand   <- popOperand
  let fallthrough = makeName "block" $ blockIndx + 1
  pure [T $ AST.Do $ AST.CondBr operand dest fallthrough []]

compileInstr (S.Block _ body) = do
  blockIndx <- blockIdentifier <$> get
  modify (\st -> st { blockIdentifier = blockIndx + 1 })
  let numberOfBlocks = countBlocks body
      name           = makeName "block" (blockIndx + numberOfBlocks)
      term           = T $ AST.Do $ AST.Br name []
      appendTerm     = appendIfLast (not . isTerm) term
  push name
  compiled <- concat <$> traverse compileInstr body
  pop
  pure $ appendTerm compiled
  -- pure compiled
    where
      pop = do
        scopeStack <- blockScopeStack <$> get
        (_, tail)  <- maybe (error "Empty scope stack") pure $ L.uncons scopeStack
        modify (\st -> st { blockScopeStack = tail })

      push :: Name.Name -> FuncGen ()
      push s = modify (\st -> st { blockScopeStack = s : blockScopeStack st })

compileInstr instr = error $ "not implemented: " ++ show instr

readScope :: Int -> FuncGen Name.Name
readScope i = do
  scopeStack <- blockScopeStack <$> get
  maybe (error "Undefined scope") (pure . snd) $ L.find ((i==) . fst) $ zip [0..] scopeStack

compileType :: S.ValueType -> Type.Type
compileType S.I32 = Type.IntegerType 32
compileType S.I64 = Type.IntegerType 64
compileType S.F32 = Type.FloatingPointType Type.FloatFP
compileType S.F64 = Type.FloatingPointType Type.DoubleFP

compileFunctionType :: S.FuncType -> FunctionType
compileFunctionType S.FuncType { S.params, S.results } = FT arguments returnType
  where
    arguments  = compileType <$> params
    returnType = compileRetTypeList results

-- seriously, consider using lens
unassignConstant :: Name.Name -> FuncGen ()
unassignConstant name = do
  st@FuncST { blockIdentifier, localConst } <- get
  let blockID    = makeName "block" blockIdentifier
      f (Just m) = Just $ M.delete blockID m
      f Nothing  = Nothing
  put $ st { localConst = M.alter f name localConst }

assignConstant :: Name.Name -> Constant.Constant -> FuncGen ()
assignConstant name const = do
  st@FuncST { blockIdentifier, localConst } <- get
  let blockID    = makeName "block" blockIdentifier
      f (Just m) = Just $ M.insert blockID const m
      f Nothing  = Just $ M.singleton blockID const
  put $ st { localConst = M.alter f name localConst }

addRenameAction :: Name.Name -> Name.Name -> FuncGen ()
addRenameAction name newName =
  modify (\st -> st { renameMap = M.insert name newName $ renameMap st })

-- pop operand off the `operandStack`
popOperand :: FuncGen AST.Operand
popOperand = do
  stack           <- operandStack <$> get
  (operand, rest) <- maybe (F.fail "not enough operands") pure $ L.uncons stack
  modify (\st -> st { operandStack = rest })
  pure operand

pushOperand :: AST.Operand -> FuncGen ()
pushOperand op = modify (\st -> st { operandStack = op : operandStack st })

pushOperands :: [AST.Operand] -> FuncGen ()
pushOperands = mapM_ pushOperand

operandType :: AST.Operand -> Type.Type
operandType (AST.LocalReference t _) = t
operandType (AST.ConstantOperand (Constant.Int bs _)) = Type.IntegerType bs
operandType (AST.ConstantOperand (Constant.Float (Float.Single _))) = Type.FloatingPointType Type.FloatFP
operandType (AST.ConstantOperand (Constant.Float (Float.Double _))) = Type.FloatingPointType Type.DoubleFP
operandType t = error $ "Not a recognized type: " ++ show t

-- create a new identifier, put the identifier on the `operandStack` and
-- increment the global identifier tracker
newInstructionConstructor :: Type.Type -> FuncGen (AST.Instruction -> AST.Named AST.Instruction)
newInstructionConstructor Type.VoidType = pure AST.Do
newInstructionConstructor idType        = do
  FuncST { localIdentifier, operandStack } <- get
  let name       = makeName "tmp" localIdentifier
      identifier = AST.LocalReference idType name
  modify (\env -> env {
      localIdentifier = localIdentifier + 1,
      operandStack = identifier : operandStack
    })
  pure (name AST.:=)

packValues :: [AST.Operand] -> AST.Operand
packValues []  = AST.LocalReference Type.VoidType "void"
packValues [x] = x
packValues l   = error "add support for multiple return vals"  -- TODO: support multiple return values

returnOperandStackItems :: FuncGen (AST.Named AST.Terminator)
returnOperandStackItems = do
  FuncST { operandStack } <- get
  let ret = do guard $ not $ L.null operandStack
               pure $ packValues (trace (show operandStack) operandStack)
  modify (\env -> env { operandStack = [] })
  pure $ AST.Do $ AST.Ret ret []

compileRetTypeList :: [S.ValueType] -> Type.Type
compileRetTypeList []  = Type.VoidType
compileRetTypeList [t] = compileType t
compileRetTypeList l   = Type.StructureType True $ compileType <$> l

-- check if a wasm instruction will be compiled to a terminator in LLVM
wasmIsTerm :: S.Instruction a -> Bool
wasmIsTerm S.Return      = True
wasmIsTerm (S.If _ _ _)  = True
wasmIsTerm (S.Br _)      = True
wasmIsTerm (S.BrIf _)    = True
wasmIsTerm (S.Block _ _) = True
wasmIsTerm _             = False

countBlocks :: [S.Instruction Natural] -> Natural
countBlocks = sum . fmap aux
  where
    aux (S.If _ b1 b2) = 3 + countBlocks b1 + countBlocks b2
    aux (S.Block _ l)  = 1 + countBlocks l
    aux (S.Br _)       = 1
    aux (S.BrIf _)     = 1
    aux _              = 0

-- compiles one WASM function into an LLVM function.
-- FIXME: rename assignments
compileFunction :: Natural -> S.Function -> ModGen Global.Global
compileFunction indx func = evalFuncGen funcGen initFuncST
  where
    initFuncST = FuncST [] 0 0 [] M.empty M.empty M.empty

    assignName (n, (t, instrs)) = (makeName "block" n, t, instrs)
    -- split the list of `LLVMObj` into blocks by looking for terminators. The
    -- terminators and the code in the corresponding blocks are then
    -- separated into an 'association list.'
    -- FIXME: splitAfter is not good enough since some blocks are nested under
    -- `if` in WASM (and possibly other instructions). These need to be
    -- un-nested into their own blocks in LLVM.
    funcGen    = do
      ModEnv { startFunctionIndex, functionTypes } <- ask
      -- compile function type information
      let FT { arguments, returnType } = functionTypes M.! (S.funcType func)
          paramList  = do
            (i, t) <- zip [0..] arguments
            pure $ Global.Parameter t (makeName "local" i) []
          parameters = (paramList, False)
          -- if indx == startIndex then "main" else "func{indx}". All the extra code
          -- handle the fact that startIndex may or may not have a value
          name = maybe (makeName "func" indx) (const "main")
            $ guard . (==indx) =<< startFunctionIndex
          localTypes = compileType <$> S.localTypes func
          localVariables = do
            (n, t) <- zip [0..] $ arguments ++ localTypes
            pure (makeName "local" n, t)
      modify (\st -> st { localVariableTypes = M.fromAscList localVariables })
      -- compile basic blocks and collect the results
      llvmInstrs   <- fmap concat $ traverse compileInstr $ Language.Wasm.Structure.body func
      remainingOps <- operandStack <$> get
      returnInstr  <- returnOperandStackItems
      consts       <- localConst <$> get
      let blks = splitAfter isTerm
               $ appendIfLast (not . isRet) (T returnInstr) (trace (show consts) llvmInstrs)
          -- TODO: conditional additional block if the last item is a block and
          -- requested a jump to an additional block
          basicBlocks = fmap (buildBlock . assignName)
                      $ zip [0..]
                      $ fromMaybe (error "empty block")
                      $ traverse unsnoc blks
      pure $ Global.functionDefaults { Global.basicBlocks
                                     , Global.name
                                     , Global.returnType
                                     , Global.parameters }

    buildBlock (name, term, instrs) = AST.BasicBlock name (unwrapI <$> instrs) (unwrapT term)

    isRet (T ((AST.:=) _ (AST.Ret _ _))) = True
    isRet _                              = False

    unwrapT (T t) = t
    unwrapT instr = error $ "Not an LLVM terminator: " ++ show instr

    unwrapI (I i) = i
    unwrapI term  = error $ "Not an LLVM instruction: " ++ show term

-- globals in the WASM sense of the term
compileGlobals :: Natural -> S.Global -> ModGen AST.Global
compileGlobals index global = globVar
  where
    globVar     = do
      init' <- init
      let initializer = Just init'
      pure $ AST.globalVariableDefaults { Global.name
                                        , Global.isConstant 
                                        , Global.type'
                                        , Global.initializer
                                        }

    name       = makeName "global" index
    isConstant = isConst $ S.globalType global
    type'      = compileType' $ S.globalType global
    exp        = compileInstr <$> S.initializer global
    init       = case compileInstr <$> S.initializer global of
                  --  [Op (Const const)] -> pure const
                   -- TODO: maybe there's a way around this? The wasm specs says
                   -- the initializer should be `expr` which could really be
                   -- anything. Not sure how to implement that into LLVM though.
                   _                  -> F.fail "initializer not constant"

    isConst (S.Const _) = True
    isConst _           = False

    compileType' (S.Const t) = compileType t
    compileType' (S.Mut   t) = compileType t

-- AST.moduleDefinitions Module
-- ASTElem = Type.StructureType false [Type.IntegerType, Type.FunctionType]

compileTable :: Natural -> S.Table -> ModGen Global.Global
compileTable index table = fail "rip"
-- compileTable index table = do
--   let instrs = [AST.Alloca (Type.NamedTypeReference "Elem") (AST.ConstantOperand 1) 8 []]
--   pure $ Global.Global {
--
--   }



-- TODO: tables, elems
-- TODO: mems
-- TODO: datas
-- TODO: imports
-- TODO: exports
compileModule :: S.Module -> Either String AST.Module
compileModule wasmMod = evalModGen modGen initModEnv
  where
    startFunctionIndex = (\(S.StartFunction n) -> n) <$> S.start wasmMod
    functionTypes      = M.fromList $ zip [0..] $ compileFunctionType <$> S.types wasmMod
    initModEnv         = ModEnv startFunctionIndex functionTypes
    wasmGlobals        = zip [0..] $ S.globals wasmMod
    wasmTables         = zip [0..] $ S.tables wasmMod
    wasmFuncs          = zip [0..] $ S.functions wasmMod
    modGen             = do
      globals <- traverse (uncurry compileGlobals) wasmGlobals
      -- tables  <- traverse (uncurry compileTable) wasmTables
      defs    <- traverse (uncurry compileFunction) wasmFuncs
      pure $ AST.defaultModule
        { AST.moduleName = "basic",
          AST.moduleDefinitions = 
            (AST.GlobalDefinition <$> defs) ++
            [
              AST.TypeDefinition "Elem" (Just (AST.StructureType False [Type.i32, Type.ptr (Type.FunctionType Type.void [] False)]))
            , AST.TypeDefinition "Table" (Just (AST.StructureType False [Type.ptr (Type.NamedTypeReference "Elem"), Type.i32, Type.i32]))
            , AST.TypeDefinition "Memory" (Just (AST.StructureType False [Type.ptr Type.i8, Type.i32, Type.i32, Type.i32]))
            ]
        }

parseModule :: B.ByteString -> Either String S.Module
parseModule = Wasm.parse
