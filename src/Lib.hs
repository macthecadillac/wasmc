{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Data.Word
import Debug.Trace

import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Structure as S
import qualified Language.Wasm.Structure (Function(..))

import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as Conv
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Float as Float
import qualified LLVM.AST.IntegerPredicate as Pred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Type as Type

import Numeric.Natural
import Utils (makeName, splitAfter)

-- a record for per-function state variables
data FuncST = FuncST { operandStack :: [AST.Operand]
                     , currentIdentifierNumber :: Natural
                     , renameMap :: M.Map Name.Name Name.Name
                     , localConst :: M.Map Name.Name Constant.Constant
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
  deriving (Functor, Applicative, Monad, MonadReader ModEnv)

newtype FuncGen a = FuncGen { runFuncGen :: StateT FuncST ModGen a }
  deriving (Functor, Applicative, Monad, MonadReader ModEnv, MonadState FuncST)

-- helper functions. Not entirely following Haskell conventions here.
evalModGen :: ModGen a -> ModEnv -> Either String a
evalModGen a = runReader (runExceptT (runModGen a))

evalFuncGen :: FuncGen a -> FuncST -> ModGen a
evalFuncGen a = evalStateT (runFuncGen a)

data LLVMInstr = B BinOp
               | U UnOp
               | Call Natural
               | SetLocal Name.Name
               | SetGlobal Name.Name
               deriving (Show)

--           tag   bitsize/fp type   type  
data BinOp = BBOOI Word32            BBOOI
           | BOOI  Word32            BOOI
           | FOOI  Type.FloatingPointType FOOI
           | OOI   Word32            OOI
           | IOOI                    IOOI

instance Show BinOp where
  show (BBOOI bs _) = show bs ++ " BBOOI"
  show (BOOI  bs _) = show bs ++ " BOOI"
  show (FOOI  fp _) = show fp ++ " FOOI"
  show (OOI   bs _) = show bs ++ "OOI"
  show (IOOI _)  = "IOOI"

data UnOp = BOI BOI

instance Show UnOp where
  show (BOI _) = "BOI"

data LLVMTerm = Ret
              | Br Natural
              -- CondBrI and CondBrB are the same on the LLVM side but the way
              -- they are treated in WASM-land is a bit different so this
              -- arrangement is for convenience
              | CondBrI [LLVMObj] [LLVMObj]
              | CondBrB Natural

instance Show LLVMTerm where
  show Ret             = "Ret"
  show (Br i)          = "Br " ++ show i
  show (CondBrI b1 b2) = "CondBrI " ++ show b1 ++ ", " ++ show b2
  show (CondBrB i)     = "CondBrB " ++ show i

type BOI =  Bool -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BOOI =  Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BBOOI = Bool -> Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type FOOI = AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type IOOI =  Pred.IntegerPredicate -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type OOI = AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type MOI = Maybe AST.Operand -> AST.InstructionMetadata -> AST.Terminator

data LLVMOp = Const     Constant.Constant
            | GlobalRef Name.Name
            | LocalRef  Name.Name
            deriving (Show)

data LLVMObj = Instr LLVMInstr
             | Op   LLVMOp
             | Term LLVMTerm
             deriving (Show)

iop :: BBOOI -> AST.Operand -> AST.Operand -> AST.Instruction
iop op a b = op False False a b []

fop :: FOOI -> AST.Operand -> AST.Operand -> AST.Instruction
fop op a b = op AST.noFastMathFlags a b []

isLLVMInstr :: LLVMObj -> Bool
isLLVMInstr (Instr _) = True
isLLVMInstr _         = False

isLLVMOp :: LLVMObj -> Bool
isLLVMOp (Op _) = True
isLLVMOp _      = False

isLLVMTerm :: LLVMObj -> Bool
isLLVMTerm (Term _) = True
isLLVMTerm _        = False

unwrapOp :: LLVMObj -> FuncGen AST.Operand 
unwrapOp (Op (Const op))   = pure $ AST.ConstantOperand op
unwrapOp (Op (LocalRef n)) = do
  FuncST { localVariableTypes, localConst } <- get
  -- `<|>` is the choice operator. It tries the first branch, and if it fails,
  -- goes on to try the second branch.
  withMsg $ const localConst <|> ref localVariableTypes
    where
      ref m = AST.LocalReference <$> M.lookup n m <*> pure n
      const m = AST.ConstantOperand <$> M.lookup n m
      withMsg = maybe (fail $ "unbound reference: " ++ show n) pure
unwrapOp a       = error $ "not an operand" ++ show a

unwrapTerm :: LLVMObj -> LLVMTerm
unwrapTerm (Term t) = t
unwrapTerm _        = error "not a terminator"

compileInstr :: S.Instruction Natural -> LLVMObj
compileInstr (S.I32Const n)  = Op $ Const $ Constant.Int 32 $ fromIntegral n
compileInstr (S.I64Const n)  = Op $ Const $ Constant.Int 64 $ fromIntegral n
compileInstr (S.F32Const n)  = Op $ Const $ Constant.Float $ Float.Single n
compileInstr (S.F64Const n)  = Op $ Const $ Constant.Float $ Float.Double n
compileInstr (S.GetGlobal n) = Op $ GlobalRef $ makeName "global" n
compileInstr (S.GetLocal n)  = Op $ LocalRef $ makeName "local" n

-- Binary Operations(IBinOPs)
-- TODO: should use helper functions instead of explicitly matching the bitsize
compileInstr (S.IBinOp S.BS32 S.IAdd)  = Instr $ B $ BBOOI 32 AST.Add
compileInstr (S.IBinOp S.BS32 S.ISub)  = Instr $ B $ BBOOI 32 AST.Sub 
compileInstr (S.IBinOp S.BS32 S.IMul)  = Instr $ B $ BBOOI 32 AST.Mul
compileInstr (S.IBinOp S.BS32 S.IDivS) = Instr $ B $ BOOI  32 AST.SDiv
compileInstr (S.IBinOp S.BS32 S.IRemS) = Instr $ B $ OOI   32 AST.SRem
compileInstr (S.IBinOp S.BS32 S.IAnd)  = Instr $ B $ OOI   32 AST.And 
compileInstr (S.IBinOp S.BS32 S.IOr)   = Instr $ B $ OOI   32 AST.Or
compileInstr (S.IBinOp S.BS32 S.IXor)  = Instr $ B $ OOI   32 AST.Xor
compileInstr (S.IBinOp S.BS32 S.IShl)  = Instr $ B $ BBOOI 32 AST.Shl

compileInstr (S.IBinOp S.BS64 S.IAdd)  = Instr $ B $ BBOOI 64 AST.Add
compileInstr (S.IBinOp S.BS64 S.ISub)  = Instr $ B $ BBOOI 64 AST.Sub 
compileInstr (S.IBinOp S.BS64 S.IMul)  = Instr $ B $ BBOOI 64 AST.Mul
compileInstr (S.IBinOp S.BS64 S.IDivS) = Instr $ B $ BOOI  64 AST.SDiv
compileInstr (S.IBinOp S.BS64 S.IRemS) = Instr $ B $ OOI   64 AST.SRem
compileInstr (S.IBinOp S.BS64 S.IAnd)  = Instr $ B $ OOI   64 AST.And 
compileInstr (S.IBinOp S.BS64 S.IOr)   = Instr $ B $ OOI   64 AST.Or
compileInstr (S.IBinOp S.BS64 S.IXor)  = Instr $ B $ OOI   64 AST.Xor
compileInstr (S.IBinOp S.BS64 S.IShl)  = Instr $ B $ BBOOI 64 AST.Shl
-- count leading zeros, trailing zeros, popped vals (IUnOPs)//////TO FIX
--compileInstr (S.IUnOp _ S.IClz) = Instr $ -- Clz
--compileInstr (S.IUnOp _ S.ICtz) = Instr $ -- Ctz
--compileInstr (S.IUnOp _ S.IPopcnt) = Instr $ -- Popcnt
-- Relational Operations(IRelOPs) IEq	 INe	 ILtU	 ILtS	 IGtU	 IGtS	 ILeU	 ILeS	 IGeU	 IGeS
compileInstr (S.IRelOp S.BS32 S.IEq)  = Instr $ B $ OOI 32 $ AST.ICmp Pred.EQ
compileInstr (S.IRelOp S.BS32 S.INe)  = Instr $ B $ OOI 32 $ AST.ICmp Pred.NE
compileInstr (S.IRelOp S.BS32 S.ILtU) = Instr $ B $ OOI 32 $ AST.ICmp Pred.ULT
compileInstr (S.IRelOp S.BS32 S.ILtS) = Instr $ B $ OOI 32 $ AST.ICmp Pred.SLT
compileInstr (S.IRelOp S.BS32 S.IGtU) = Instr $ B $ OOI 32 $ AST.ICmp Pred.UGT
compileInstr (S.IRelOp S.BS32 S.IGtS) = Instr $ B $ OOI 32 $ AST.ICmp Pred.SGT
compileInstr (S.IRelOp S.BS32 S.ILeU) = Instr $ B $ OOI 32 $ AST.ICmp Pred.ULE
compileInstr (S.IRelOp S.BS32 S.ILeS) = Instr $ B $ OOI 32 $ AST.ICmp Pred.SLE
compileInstr (S.IRelOp S.BS32 S.IGeU) = Instr $ B $ OOI 32 $ AST.ICmp Pred.UGE
compileInstr (S.IRelOp S.BS32 S.IGeS) = Instr $ B $ OOI 32 $ AST.ICmp Pred.SGE

compileInstr (S.IRelOp S.BS64 S.IEq)  = Instr $ B $ OOI 64 $ AST.ICmp Pred.EQ
compileInstr (S.IRelOp S.BS64 S.INe)  = Instr $ B $ OOI 64 $ AST.ICmp Pred.NE
compileInstr (S.IRelOp S.BS64 S.ILtU) = Instr $ B $ OOI 64 $ AST.ICmp Pred.ULT
compileInstr (S.IRelOp S.BS64 S.ILtS) = Instr $ B $ OOI 64 $ AST.ICmp Pred.SLT
compileInstr (S.IRelOp S.BS64 S.IGtU) = Instr $ B $ OOI 64 $ AST.ICmp Pred.UGT
compileInstr (S.IRelOp S.BS64 S.IGtS) = Instr $ B $ OOI 64 $ AST.ICmp Pred.SGT
compileInstr (S.IRelOp S.BS64 S.ILeU) = Instr $ B $ OOI 64 $ AST.ICmp Pred.ULE
compileInstr (S.IRelOp S.BS64 S.ILeS) = Instr $ B $ OOI 64 $ AST.ICmp Pred.SLE
compileInstr (S.IRelOp S.BS64 S.IGeU) = Instr $ B $ OOI 64 $ AST.ICmp Pred.UGE
compileInstr (S.IRelOp S.BS64 S.IGeS) = Instr $ B $ OOI 64 $ AST.ICmp Pred.SGE
-- FUn Operations(FUnOPs) FAbs	 FNeg	 FCeil	 FFloor	 FTrunc	 FNearest	 FSqrt
--compileInstr (S.FUnOp _ S.FAbs) = Instr $ IOOI Abs

compileInstr (S.SetGlobal n)          = Instr $ SetGlobal $ makeName "global" n
compileInstr (S.SetLocal n)           = Instr $ SetLocal  $ makeName "local" n
compileInstr (S.Call i)               = Instr $ Call $ fromIntegral i

-- Terminators (return, br etc)
compileInstr S.Return       = Term Ret
-- TODO: must be handled by the split block routine since `b1` and `b2` are
-- blocks by themselves.
compileInstr (S.If _ b1 b2) = Term $ CondBrI (compileInstr <$> b1) (compileInstr <$> b2)
compileInstr (S.Br i)       = Term $ Br i
-- TODO: might need to be handled by the split block routine. WASM br_if will
-- fall through if the bool is false
compileInstr (S.BrIf i)     = Term $ CondBrB i
compileInstr instr = error $ "Not implemented in compileInstr: " ++ show instr

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

-- this builds a basic block. A basic block is a single-entry-single-exit block
-- of code. Here it means it is a block of code that has an identifier and a
-- terminator (see the llvm-hs-pure docs for specifics) as its last instruction.
-- `llvmObjs` here is a list of `LLVMObj` that fits within one basic block--that
-- is to say it must not contain terminators. The single terminator is
-- separately passed in to the function.
buildBasicBlock :: Name.Name -> Maybe LLVMTerm -> [LLVMObj] -> FuncGen Global.BasicBlock
buildBasicBlock name term llvmObjs = Global.BasicBlock name <$> llvmInstrs <*> makeTerminator term
  where
    -- we define a chunk as a block of code that ends with an LLVM instruction.
    -- This excludes consts, set and get in the WASM world. Here we divide
    -- `llvmObjs` into a list of chunks.
    chunks = splitAfter isLLVMInstr llvmObjs
    -- we consume the chunks by munching them one at a time, collecting the
    -- result with `traverse` which 'magically' handles the `ModGen` monad, and
    -- concatenating the results into a list of LLVM instructions.
    llvmInstrs = renameLocals =<< L.concat <$> traverse munch chunks

    -- For `return`s, WASM always returns whatever is left on the stack at the
    -- time of its invocation, whereas LLVM is very explicit about what to
    -- return when `ret` is called, so we need to gather what is on the
    -- `operandStack` and feed them to `AST.Ret`.
    -- TODO: we will need to expand this when we implement more terminators
    makeTerminator :: Maybe LLVMTerm -> FuncGen (AST.Named AST.Terminator)
    makeTerminator Nothing       = returnOperandStackItems
    makeTerminator (Just Ret)    = returnOperandStackItems
    makeTerminator (Just (Br i)) = pure $ AST.Do $ AST.Br (makeName "block" i) []
    makeTerminator _             = error "not implemented"

    returnOperandStackItems = do
      FuncST { operandStack } <- get
      let ret = do guard $ not $ L.null operandStack
                   pure $ packValues operandStack
      modify (\env -> env { operandStack = [] })
      pure $ AST.Do $ AST.Ret ret []

    packValues []  = AST.LocalReference Type.VoidType "void"
    packValues [x] = x
    packValues l   = error "add support for multiple return vals"  -- TODO: support multiple return values

    -- rename identifiers according to the rename map built during `munch`.
    renameLocals :: [AST.Named AST.Instruction] -> FuncGen [AST.Named AST.Instruction]
    renameLocals l = do
      FuncST { renameMap } <- get
      pure $ rename renameMap <$> l
      where
        rename map ((AST.:=) name instr) = (fromMaybe name $ M.lookup name map) AST.:= instr

    -- This takes a 'chunk' of the block and turns it into a single LLVM
    -- instruction.
    -- TODO: check the order at which WASM pops items off the stack and feeds
    -- them to the instructions--just check whether the behavior of `sub` in
    -- WASM matches that of the generated code.
    munch :: [LLVMObj] -> FuncGen [AST.Named AST.Instruction]
    -- the necessity of munching backwards has to do with how a linked-list
    -- is a representation of a stack
    munch = munchBackwards . L.reverse
      where
        munchBackwards [] = pure []
        munchBackwards (Instr (B op):rest) = do
          -- merge the explicitly provided operands in the current chunk with
          -- the `operandStack` then peel off just the right amount of operands
          -- for the current instruction
          pushOperands =<< traverse unwrapOp rest
          a          <- popOperand
          b          <- popOperand
          -- generate a new identifier for the intermediate result. In LLVM IR
          -- this amounts to saving the results to a 'variable.'
          identifier <- identify $ B op
          pure [identifier AST.:= buildBinOp op a b]
        -- for local.set instructions, we only have to change previously
        -- assigned identifiers to the one corresponding to the target index.
        -- We'll need to do that after the entire block is built.
        munchBackwards (Instr (SetLocal n):rest) = do
          pushOperands =<< traverse unwrapOp rest
          -- if there is a constant associated to the variable, remove it
          unassignConstant n
          a <- popOperand
          case a of
            -- for references, this is acieved by renaming identifiers at the
            -- end
            (AST.LocalReference _ name) -> addRenameAction name n *> pure []
            (AST.ConstantOperand const) -> assignConstant n const *> pure []
            _                           -> error "unsupported operand"
        munchBackwards (Instr (Call i):rest) = do
          pushOperands =<< traverse unwrapOp rest
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
          identifier <- newIdentifier returnType
          pure [identifier AST.:= instr]
        munchBackwards l
            -- any chunk that does not end with an instruction is likely the
            -- last chunk in the basic block and will be returned. Put them on
            -- the `operandStack` and the `buildBasicBlock` function will deal
            -- with the rest.
          | all isLLVMOp l = traverse unwrapOp l >>= pushOperands >> pure []
          | otherwise      = error $ "AST.Instruction sequence not implemented: " ++ show l -- FIXME: complete me!!

    -- seriously, consider using lens
    unassignConstant :: Name.Name -> FuncGen ()
    unassignConstant name = do
      st@FuncST { localConst } <- get
      put $ st { localConst = M.delete name localConst }

    assignConstant :: Name.Name -> Constant.Constant -> FuncGen ()
    assignConstant name const = do
      st@FuncST { localConst } <- get
      put $ st { localConst = M.insert name const localConst }

    addRenameAction :: Name.Name -> Name.Name -> FuncGen ()
    addRenameAction name newName = do
      st@FuncST { renameMap } <- get
      put $ st { renameMap = M.insert name newName renameMap }

    -- pop operand off the `operandStack`
    popOperand :: FuncGen AST.Operand
    popOperand = do
      stack           <- operandStack <$> get
      (operand, rest) <- maybe (fail "not enough operands") pure $ L.uncons stack
      modify (\env -> env { operandStack = rest })
      pure operand

    pushOperand :: AST.Operand -> FuncGen ()
    pushOperand op = modify (\env -> env { operandStack = op : operandStack env })

    pushOperands :: [AST.Operand] -> FuncGen ()
    pushOperands = sequence_ . fmap pushOperand

    buildBinOp :: BinOp -> AST.Operand -> AST.Operand -> AST.Instruction
    buildBinOp (BBOOI _ op) = iop op
    buildBinOp (FOOI  _ op) = fop op
    buildBinOp o          = error $ "Not implemented for binop type: " ++ show o

    identify :: LLVMInstr -> FuncGen Name.Name
    identify (B (BBOOI bs _)) = newIdentifier $ Type.IntegerType bs
    identify (B (BOOI  bs _)) = newIdentifier $ Type.IntegerType bs
    identify (B (OOI   bs _)) = newIdentifier $ Type.IntegerType bs
    identify _                = error "not implmented"

    -- create a new identifier, put the identifier on the `operandStack` and
    -- increment the global identifier tracker
    newIdentifier :: Type.Type -> FuncGen Name.Name
    newIdentifier Type.VoidType = pure $ Name.mkName "void"
    newIdentifier idType        = do
      FuncST { currentIdentifierNumber, operandStack } <- get
      let name       = makeName "tmp" $ currentIdentifierNumber
          identifier = AST.LocalReference idType name
      modify (\env -> env {
          currentIdentifierNumber = currentIdentifierNumber + 1,
          operandStack = identifier : operandStack
        })
      pure name

compileRetTypeList :: [S.ValueType] -> Type.Type
compileRetTypeList []  = Type.VoidType
compileRetTypeList [t] = compileType t
compileRetTypeList l   = Type.StructureType True $ compileType <$> l

-- compiles one WASM function into an LLVM function.
compileFunction :: Natural -> S.Function -> ModGen Global.Global
compileFunction indx func = evalFuncGen funcGen initFuncST
  where
    initFuncST = FuncST [] 0 M.empty M.empty M.empty

    assignName (n, (t, instrs)) = (makeName "block" n, t, instrs)
    llvmObjs   = compileInstr <$> Language.Wasm.Structure.body func
    -- split the list of `LLVMObj` into blocks by looking for terminators. The
    -- terminators and the code in the corresponding blocks are then
    -- separated into an 'association list.'
    -- FIXME: splitAfter is not good enough since some blocks are nested under
    -- `if` in WASM (and possibly other instructions). These need to be
    -- un-nested into their own blocks in LLVM.
    blks       = splitTerm <$> splitAfter isLLVMTerm llvmObjs  -- FIXME: questionable criteria here
    namedBlks  = assignName <$> zip [0..] blks
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
      basicBlocks <- traverse (\(n, t, o) -> buildBasicBlock n t o) namedBlks
      pure $ Global.functionDefaults { Global.basicBlocks
                                     , Global.name
                                     , Global.returnType
                                     , Global.parameters }

    -- split the last `LLVMObj` of a list of objects if it is a terminator.
    splitTerm :: [LLVMObj] -> (Maybe LLVMTerm, [LLVMObj])
    splitTerm []            = error "empty block"  -- this would be a bug
    splitTerm (Term t:rest) = (Just t, L.reverse rest)
    splitTerm instrs        = (Nothing, instrs)

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
                   [Op (Const const)] -> pure const
                   -- TODO: maybe there's a way around this? The wasm specs says
                   -- the initializer should be `expr` which could really be
                   -- anything. Not sure how to implement that into LLVM though.
                   _                  -> fail "initializer not constant"

    isConst (S.Const _) = True
    isConst _           = False

    compileType' (S.Const t) = compileType t
    compileType' (S.Mut   t) = compileType t

-- see todonotes for the eponymous
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
    wasmFuncs          = zip [0..] $ S.functions wasmMod
    modGen             = do
      globals <- traverse (uncurry compileGlobals) wasmGlobals
      defs    <- traverse (uncurry compileFunction) wasmFuncs
      pure $ AST.defaultModule
        { AST.moduleName = "basic",
          AST.moduleDefinitions = AST.GlobalDefinition <$> defs
        }

parseModule :: B.ByteString -> Either String S.Module
parseModule = Wasm.parse
