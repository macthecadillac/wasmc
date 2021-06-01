{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Control.Monad.Fail as F
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
import qualified Language.Wasm.Structure (Function(..), MemArg(..))

import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as Addr
import qualified LLVM.AST.CallingConvention as Conv
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Float as Float
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Type as Type
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Visibility as Visibility

import Mem
import Intrinsics
import Gen
import Numeric.Natural
import Utils (appendIfLast, makeName, splitAfter, unsnoc, operandType, toLog)
import qualified LLVM.AST as AST.Instruction

type BOI    = Bool -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BOOI   = Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BBOOI  = Bool -> Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type FOOI   = AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type POOI p = p -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type OOI    = AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type MOI    = Maybe AST.Operand -> AST.InstructionMetadata -> AST.Terminator
type OTI    = AST.Operand -> Type.Type -> AST.InstructionMetadata -> AST.Instruction

type UnOpBuilder a = a -> AST.Operand -> AST.Instruction
type BinOpBuilder a = a -> AST.Operand -> AST.Operand -> AST.Instruction
type CmpBuilder p = POOI p -> p -> AST.Operand -> AST.Operand -> AST.Instruction
-- allocatedType :: Type	 
-- numElements :: Maybe Operand	 
-- alignment :: Word32	 
-- metadata :: InstructionMetadata	 

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

sBitSize :: S.BitSize -> String
sBitSize S.BS32 = "32"
sBitSize S.BS64 = "64"

nBitSize :: S.BitSize -> Word32
nBitSize S.BS32 = 32
nBitSize S.BS64 = 64

iBitSize :: S.BitSize -> Type.Type
iBitSize S.BS32 = Type.i32
iBitSize S.BS64 = Type.i64

fBitSize :: S.BitSize -> Type.Type
fBitSize S.BS32 = Type.float
fBitSize S.BS64 = Type.double

-- check if a wasm instruction will be compiled to a terminator in LLVM
wasmIsTerm :: S.Instruction a -> Bool
wasmIsTerm S.Unreachable = True
wasmIsTerm S.Return      = True
wasmIsTerm S.If {}       = True
wasmIsTerm (S.Br _)      = True
wasmIsTerm (S.BrIf _)    = True
wasmIsTerm (S.Block _ _) = True
wasmIsTerm (S.Loop _ _)  = True
wasmIsTerm _             = False

countTerminals :: [S.Instruction Natural] -> Natural
countTerminals = sum . fmap aux
  where
    aux (S.If _ b1 b2) = oneIfTailIsNotTerm b1 + oneIfTailIsNotTerm b2 + countTerminals b1 + countTerminals b2
    aux (S.Block _ l)  = oneIfTailIsNotTerm l + countTerminals l
    aux (S.Br _)       = 1
    aux (S.BrIf _)     = 1
    aux _              = 0

    oneIfTailIsNotTerm = oneIfHead (not . wasmIsTerm) . reverse

    oneIfHead _ []    = 0
    oneIfHead g (x:_) | g x       = 1
                      | otherwise = 0

compileConst :: Constant.Constant -> InstrGen [LLVMInstr]
compileConst const = do
  tell $ toLog "    constant: " [const]
  pushOperand $ AST.ConstantOperand const
  pure []

compileBinOp :: BinOpBuilder a -> a -> Type.Type -> InstrGen [LLVMInstr]
compileBinOp builder op t = do
  (phiB, b)   <- popOperand
  (phiA, a)   <- popOperand
  -- generate a new identifier for the intermediate result. In LLVM IR
  -- this amounts to saving the results to a 'variable.'
  constructor <- newInstructionConstructor t
  let instrs = phiB ++ phiA ++ [constructor $ builder op a b]
  tell $ toLog "    emit: " instrs
  pure $ fmap I instrs

compileUnOp :: UnOpBuilder a -> a -> Type.Type -> InstrGen [LLVMInstr]
compileUnOp builder op t = do
  (phi, a)  <- popOperand
  constructor <- newInstructionConstructor t
  let instrs = phi ++ [constructor $ builder op a]
  tell $ toLog "    emit: " instrs
  pure $ fmap I instrs

compileCmpOp :: POOI p -> p -> InstrGen [LLVMInstr]
compileCmpOp cmp pred = do
  (phiB, b)   <- popOperand
  (phiA, a)   <- popOperand
  -- comparison operators return i1 sized booleans
  constructor <- newInstructionConstructor Type.i1
  let instrs = phiB ++ phiA ++ [constructor $ pooi cmp pred a b]
  tell $ toLog "    emit: " instrs
  pure $ fmap I instrs
  
compileCastOp :: OTI -> Type.Type -> InstrGen [LLVMInstr]
compileCastOp op t = do
  (phi, a)    <- popOperand
  constructor <- newInstructionConstructor t
  let instrs = phi ++ [constructor $ op a t []]
  tell $ toLog "    emit: " instrs
  pure $ fmap I instrs

compileFunctionCall :: Name.Name -> [Type.Type] -> Type.Type -> InstrGen [LLVMInstr]
compileFunctionCall name args = aux
  where
    -- `<>` is the "summation" operator over the semigroup. It's a
    -- generalization of `++` for lists and `+`/`*` for numbers
    aux retType@Type.StructureType { Type.elementTypes } = alloc <> singleRet <> pushStack
      where
        ptrTy  = Type.ptr retType -- default addr

        alloc = do
          constr <- newInstructionConstructor ptrTy
          let allocInstr = constr $ AST.Alloca ptrTy Nothing 0 []
          tell $ toLog "    call-emit: " [allocInstr]
          pure [I allocInstr]

        singleRet = do
          callInstrs <- singleReturnCall (args ++ [ptrTy]) Type.VoidType
          tell $ toLog "               " callInstrs
          pure callInstrs

        pushStack = do
          (_, ptr) <- popOperand  -- no phi since there is no branching since alloc
          let genInstr (i, t) = do
                let idx          = AST.ConstantOperand $ Constant.Int 32 i
                    unamedGetPtr = AST.GetElementPtr False ptr [idx] []
                    ptrType      = Type.PointerType t $ Addr.AddrSpace 0
                ptrInstr  <- newInstructionConstructor ptrType <*> pure unamedGetPtr
                (_, addr) <- popOperand  -- no phi
                loadInstr <- newInstructionConstructor t <*> pure (AST.Load False addr Nothing 0 [])
                pure [ptrInstr, loadInstr]
          pushStackInstrs <- foldMap genInstr $ zip [0..] elementTypes
          tell $ toLog "               " pushStackInstrs
          pure $ fmap I pushStackInstrs

    aux retType = singleReturnCall args retType

    singleReturnCall arguments returnType = do
      let nArgs    = fromIntegral (L.length arguments)
          function = AST.ConstantOperand
                   $ flip Constant.GlobalReference name 
                   $ Type.FunctionType returnType arguments False
      -- pop the required number of operands off the `operandStack` and
      -- collect the results into a list. `replicateM` deals with the
      -- InstrGen monad.
      ops         <- replicateM (fromIntegral nArgs) popOperand
      let (phis, args) = unzip ops
          arguments    = reverse [(operand, []) | operand <- args]
          instr        = AST.Call Nothing Conv.C [] (Right function) arguments [] []
      constructor <- newInstructionConstructor returnType
      let instrs = concat phis ++ [constructor instr]
      pure $ fmap I instrs

callIntrinsics :: String -> InstrGen [LLVMInstr]
callIntrinsics name = do
  let FT { arguments, returnType } = llvmIntrinsicsTypes M.! name
  compileFunctionCall (Name.mkName name) arguments returnType

compileInstr :: S.Instruction Natural -> InstrGen [LLVMInstr]
compileInstr S.Unreachable          = pure [T $ AST.Do $ AST.Unreachable []]
compileInstr S.Nop                  = pure []
compileInstr S.Drop                 = popOperand $> []

compileInstr (S.IUnOp bs S.IClz)    = pushOperand false *> callIntrinsics ("llvm.ctlz.i" ++ sBitSize bs)
compileInstr (S.IUnOp bs S.ICtz)    = pushOperand false *> callIntrinsics ("llvm.cttz.i" ++ sBitSize bs)
compileInstr (S.IUnOp bs S.IPopcnt) = callIntrinsics $ "llvm.ctpop.i" ++ sBitSize bs

compileInstr (S.FUnOp bs S.FAbs)     = callIntrinsics $ "llvm.fabs.f" ++ sBitSize bs
compileInstr (S.FUnOp S.BS32 S.FNeg) = compileInstr (S.F32Const (-1)) <> compileInstr (S.FBinOp S.BS32 S.FMul)
compileInstr (S.FUnOp S.BS64 S.FNeg) = compileInstr (S.F64Const (-1)) <> compileInstr (S.FBinOp S.BS64 S.FMul)
compileInstr (S.FUnOp bs S.FCeil)    = callIntrinsics $ "llvm.ceil.f" ++ sBitSize bs
compileInstr (S.FUnOp bs S.FFloor)   = callIntrinsics $ "llvm.floor.f" ++ sBitSize bs
compileInstr (S.FUnOp bs S.FTrunc)   = callIntrinsics $ "llvm.trunc.f" ++ sBitSize bs
compileInstr (S.FUnOp bs S.FNearest) = callIntrinsics $ "llvm.roundeven.f" ++ sBitSize bs
compileInstr (S.FUnOp bs S.FSqrt)    = callIntrinsics $ "llvm.sqrt.f" ++ sBitSize bs

compileInstr S.I32Eqz                = compileInstr (S.I32Const 0) <> compileInstr (S.IRelOp S.BS32 S.IEq)
compileInstr S.I64Eqz                = compileInstr (S.I64Const 0) <> compileInstr (S.IRelOp S.BS64 S.IEq)

-- not sure how to wrap integers in LLVM
-- compileInstr S.I32WrapI64            = throwError "not implemented: S.I32WrapI64"
compileInstr S.I64ExtendSI32         = compileCastOp AST.SExt Type.i64
compileInstr S.I64ExtendUI32         = compileCastOp AST.ZExt Type.i64
compileInstr (S.ITruncFU _ bs)       = compileCastOp AST.SIToFP $ iBitSize bs
compileInstr (S.ITruncFS _ bs)       = compileCastOp AST.UIToFP $ iBitSize bs
compileInstr (S.FConvertIU _ bs)     = compileCastOp AST.FPToUI $ iBitSize bs
compileInstr (S.FConvertIS _ bs)     = compileCastOp AST.FPToSI $ iBitSize bs
compileInstr S.F32DemoteF64          = compileCastOp AST.FPTrunc Type.float
compileInstr S.F64PromoteF32         = compileCastOp AST.FPExt Type.double
compileInstr (S.IReinterpretF bs)    = compileCastOp AST.BitCast $ iBitSize bs
compileInstr (S.FReinterpretI bs)    = compileCastOp AST.BitCast $ iBitSize bs

compileInstr (S.IBinOp bs S.IAdd)  = compileBinOp bbooi AST.Add  $ iBitSize bs
compileInstr (S.IBinOp bs S.ISub)  = compileBinOp bbooi AST.Sub  $ iBitSize bs
compileInstr (S.IBinOp bs S.IMul)  = compileBinOp bbooi AST.Mul  $ iBitSize bs
compileInstr (S.IBinOp bs S.IDivS) = compileBinOp booi  AST.SDiv $ iBitSize bs
compileInstr (S.IBinOp bs S.IDivU) = compileBinOp booi  AST.UDiv $ iBitSize bs
compileInstr (S.IBinOp bs S.IRemS) = compileBinOp ooi   AST.SRem $ iBitSize bs
compileInstr (S.IBinOp bs S.IAnd)  = compileBinOp ooi   AST.And  $ iBitSize bs
compileInstr (S.IBinOp bs S.IOr)   = compileBinOp ooi   AST.Or   $ iBitSize bs
compileInstr (S.IBinOp bs S.IXor)  = compileBinOp ooi   AST.Xor  $ iBitSize bs
compileInstr (S.IBinOp bs S.IShl)  = compileBinOp bbooi AST.Shl  $ iBitSize bs
compileInstr (S.IBinOp bs S.IShrU) = compileBinOp booi  AST.LShr $ iBitSize bs
compileInstr (S.IBinOp bs S.IShrS) = compileBinOp booi  AST.AShr $ iBitSize bs

compileInstr (S.IRelOp _  S.IEq)   = compileCmpOp AST.ICmp IPred.EQ
compileInstr (S.IRelOp _  S.INe)   = compileCmpOp AST.ICmp IPred.NE
compileInstr (S.IRelOp _  S.ILtU)  = compileCmpOp AST.ICmp IPred.ULT
compileInstr (S.IRelOp _  S.ILtS)  = compileCmpOp AST.ICmp IPred.SLT
compileInstr (S.IRelOp _  S.IGtU)  = compileCmpOp AST.ICmp IPred.UGT
compileInstr (S.IRelOp _  S.IGtS)  = compileCmpOp AST.ICmp IPred.SGT
compileInstr (S.IRelOp _  S.ILeU)  = compileCmpOp AST.ICmp IPred.ULE
compileInstr (S.IRelOp _  S.ILeS)  = compileCmpOp AST.ICmp IPred.SLE
compileInstr (S.IRelOp _  S.IGeU)  = compileCmpOp AST.ICmp IPred.UGE
compileInstr (S.IRelOp _  S.IGeS)  = compileCmpOp AST.ICmp IPred.SGE

compileInstr (S.FBinOp bs S.FAdd)  = compileBinOp fooi AST.FAdd $ fBitSize bs
compileInstr (S.FBinOp bs S.FSub)  = compileBinOp fooi AST.FSub $ fBitSize bs
compileInstr (S.FBinOp bs S.FMul)  = compileBinOp fooi AST.FMul $ fBitSize bs
compileInstr (S.FBinOp bs S.FDiv)  = compileBinOp fooi AST.FDiv $ fBitSize bs
compileInstr (S.FBinOp bs S.FMin)  = callIntrinsics $ "llvm.minnum.f" ++ sBitSize bs
compileInstr (S.FBinOp bs S.FMax)  = callIntrinsics $ "llvm.maxnum.f" ++ sBitSize bs
-- value of the first argument, sign of the second argument
compileInstr (S.FBinOp bs S.FCopySign) = callIntrinsics $ "llvm.copysign.f" ++ sBitSize bs

compileInstr (S.FRelOp _  S.FEq)   = compileCmpOp AST.FCmp FPred.OEQ
compileInstr (S.FRelOp _  S.FNe)   = compileCmpOp AST.FCmp FPred.ONE
compileInstr (S.FRelOp _  S.FLt)   = compileCmpOp AST.FCmp FPred.OLT
compileInstr (S.FRelOp _  S.FGt)   = compileCmpOp AST.FCmp FPred.OGT
compileInstr (S.FRelOp _  S.FLe)   = compileCmpOp AST.FCmp FPred.OLE
compileInstr (S.FRelOp _  S.FGe)   = compileCmpOp AST.FCmp FPred.OGE

compileInstr S.Select              = do
  (phiC, cond)  <- popOperand
  (phiF, false) <- popOperand
  (phiT, true)  <- popOperand
  constructor   <- newInstructionConstructor $ operandType true
  let instrs = phiC ++ phiF ++ phiT ++ [constructor $ AST.Select cond true false []]
  tell $ toLog "    emit: " instrs
  pure $ fmap I instrs

compileInstr (S.I32Const n)        = compileConst $ Constant.Int 32 $ fromIntegral n
compileInstr (S.I64Const n)        = compileConst $ Constant.Int 64 $ fromIntegral n
compileInstr (S.F32Const n)        = compileConst $ Constant.Float $ Float.Single n
compileInstr (S.F64Const n)        = compileConst $ Constant.Float $ Float.Double n

compileInstr (S.SetGlobal n)       = throwError "not implemented: SetGlobal"
compileInstr (S.TeeLocal n)        = do
  -- if there is a constant associated to the variable, remove it
  let ident = makeName "local" n
  unassignConstant ident
  (phi, a) <- peekOperand
  case a of
    (AST.LocalReference _ name) -> addRenameAction name ident
    (AST.ConstantOperand const) -> assignConstant ident const
    _                           -> throwError "unsupported operand"
  tell ["    emit: " ++ show phi]
  pure $ I <$> phi

compileInstr (S.SetLocal n)        = compileInstr (S.TeeLocal n) <> compileInstr S.Drop
compileInstr (S.GetGlobal n)       = throwError "not implemented: GetGlobal"

-- `<|>` is the choice operator. It tries the first branch, and if it fails,
-- goes on to try the second branch.
compileInstr (S.GetLocal n)        = inBlockConst <|> outOfBlockConst <|> reference
  where
    inBlockConst    = do
      constMap  <- gets localConst
      blockID   <- gets $ makeName "block" . blockIdentifier
      constants <- withMsg $ M.lookup name constMap
      c         <- liftMaybe "no constants found." $ M.lookup blockID constants
      pushOperand (AST.ConstantOperand c)
      pure []

    outOfBlockConst = do
      InstrST { localVariableTypes, localConst } <- get
      blockID     <- gets $ makeName "block" . blockIdentifier
      varType     <- withMsg $ M.lookup name localVariableTypes
      constants   <- withMsg $ M.lookup name localConst
      case M.assocs constants of
        []       -> throwError "no constants found"
        [(_, c)] -> pushOperand (AST.ConstantOperand c) $> []
        l        -> do constructor <- newInstructionConstructor varType
                       let operand = first AST.ConstantOperand . swap <$> l
                           instrs  = [constructor $ AST.Phi varType operand []]
                       tell $ toLog "    emit: " instrs
                       pure $ I <$> instrs

    reference = do
      InstrST { localVariableTypes } <- get
      operand <- withMsg $ AST.LocalReference <$> M.lookup name localVariableTypes <*> pure name
      pushOperand operand
      pure []

    name      = makeName "local" n
    liftMaybe = \msg -> maybe (throwError msg) pure
    withMsg   = liftMaybe $ "unbound reference: " ++ show name

compileInstr (S.Call i) = do
  ModEnv { functionTypes } <- ask
  let FT { arguments, returnType } = functionTypes M.! makeName "func" i
      name                         = makeName "func" i
  compileFunctionCall name arguments returnType

compileInstr (S.CallIndirect i) = do
  funcTypeMap <- asks functionTypeIndices
  funcType    <- liftMaybe $ M.lookup i funcTypeMap
  (phiI, idx) <- popOperand
  tableRef    <- asks tableReference
  let zero    = AST.ConstantOperand $ Constant.Int 32 0
      getAddr = AST.GetElementPtr True tableRef [zero, idx] []  -- address of the function
      funcPtrType = Type.ptr $ llvmize funcType
      nArgs       = fromIntegral $ L.length $ arguments funcType
  ptrInstr    <- newNamedInstruction (Type.ptr funcPtrType) getAddr
  (_, addr)   <- popOperand  -- no phi
  loadInstr   <- newNamedInstruction funcPtrType $ AST.Load False addr Nothing 0 []
  (_, func)   <- popOperand
  ops         <- replicateM (fromIntegral nArgs) popOperand
  let (phis, args) = unzip ops
      arguments    = reverse [(operand, []) | operand <- args]
      retType      = returnType funcType
  callInstr   <- newNamedInstruction retType $ AST.Call Nothing Conv.C [] (Right func) arguments [] []
  pure $ fmap I [ptrInstr, loadInstr, callInstr]
    where
      llvmize FT { arguments, returnType } = Type.FunctionType returnType arguments False
      liftMaybe = maybe (throwError "not a known function type") pure

compileInstr S.Return = do
  (phi, term) <- returnOperandStackItems
  blockID     <- gets $ makeName "block" . blockIdentifier
  deleteOperandStack blockID
  incrBlockIdentifier
  tell $ toLog "    return-emit: " phi
  tell $ toLog "    return-term: " [term]
  pure $ (I <$> phi) ++ [T term]

compileInstr (S.If _ b1 b2) = do
  blockIndx <- gets blockIdentifier
  let b1Count    = countTerminals b1
      b2Count    = countTerminals b2
      originID   = makeName "block" blockIndx
      b1Ident    = makeName "block" $ blockIndx + 1
      b2Ident    = makeName "block" $ blockIndx + b1Count + 2
      endIf      = makeName "block" $ blockIndx + b1Count + b2Count + 3
      appendTerm = appendIfLast (not . wasmIsTerm) (S.Br 0)
  (phiP, operand) <- popOperand
  incrBlockIdentifier
  pushScope endIf
  thenInstrs      <- branchOperandStack originID b1Ident
  b1Compiled      <- foldMap compileInstr $ appendTerm b1
  _               <- moveOperandStack originID b2Ident  -- same instrs as thenInstrs
  b2Compiled      <- foldMap compileInstr $ appendTerm b2
  popScope
  let term = T $ AST.Do $ AST.CondBr operand b1Ident b2Ident []
  tell $ toLog "    if-emit: " $ phiP ++ thenInstrs
  tell $ toLog "    if-term: " [term]
  pure $ fmap I phiP ++ fmap I thenInstrs ++ [term] ++ b1Compiled ++ b2Compiled

-- the index here is the levels of scopes. It's a WASM peculiarity
compileInstr (S.Br i)       = do
  origin   <- gets $ makeName "block" . blockIdentifier
  dest     <- readScope $ fromIntegral i
  brInstrs <- moveOperandStack origin dest
  incrBlockIdentifier
  let term = AST.Do $ AST.Br dest []
  tell $ toLog "    br-emit: " brInstrs
  tell $ toLog "    br-term: " [term]
  pure $ fmap I brInstrs ++ [T term]

compileInstr (S.BrIf i)     = do
  blockIndx       <- gets blockIdentifier
  dest            <- readScope $ fromIntegral i
  (phiP, operand) <- popOperand
  let origin      = makeName "block" blockIndx
      fallthrough = makeName "block" $ blockIndx + 1
  brInstrs        <- branchOperandStack origin dest
  _               <- moveOperandStack origin fallthrough
  incrBlockIdentifier
  let term = AST.Do $ AST.CondBr operand dest fallthrough []
  tell $ toLog "    brif-emit: " $ phiP ++ brInstrs
  tell $ toLog "    brif-term: " [term]
  pure $ fmap I phiP ++ fmap I brInstrs ++ [T term]

compileInstr (S.Block _ body) = do
  tell ["  begin block"]
  blockIndx <- gets blockIdentifier
  let wasmInstrs     = appendIfLast (not . wasmIsTerm) (S.Br 0) body
      numberOfBlocks = countTerminals wasmInstrs
      endOfBlock     = makeName "block" $ blockIndx + numberOfBlocks
  pushScope endOfBlock
  compiled <- foldMap compileInstr wasmInstrs
  popScope
  tell ["  end block"]
  pure compiled

-- the difference with Block is where the scope starts
compileInstr (S.Loop _ body) = do
  tell ["  begin loop"]
  blockIndx <- gets blockIdentifier
  let wasmInstrs     = appendIfLast (not . wasmIsTerm) (S.Br 1) body
      numberOfBlocks = countTerminals wasmInstrs
      startOfBlock   = makeName "block" $ blockIndx + 1
      endOfBlock     = makeName "block" $ blockIndx + numberOfBlocks + 1
      start          = T $ AST.Do $ AST.Br startOfBlock []
  pushScope endOfBlock
  pushScope startOfBlock
  compiled <- foldMap compileInstr wasmInstrs
  popScope
  popScope
  tell ["  end loop"]
  pure $ start : compiled

compileInstr (S.I32Load    (S.MemArg _ algn)) = compileLoadOp algn Type.i32
compileInstr (S.I64Load    (S.MemArg _ algn)) = compileLoadOp algn Type.i64
compileInstr (S.F32Load    (S.MemArg _ algn)) = compileLoadOp algn Type.float
compileInstr (S.F64Load    (S.MemArg _ algn)) = compileLoadOp algn Type.double
-- load one byte as i8, then sign extend to i32. `<>` in this context means
-- "then"
compileInstr (S.I32Load8S  (S.MemArg _ algn)) = compileLoadOp algn Type.i8  <> compileCastOp AST.SExt Type.i32
compileInstr (S.I32Load8U  (S.MemArg _ algn)) = compileLoadOp algn Type.i8  <> compileCastOp AST.ZExt Type.i32
compileInstr (S.I32Load16S (S.MemArg _ algn)) = compileLoadOp algn Type.i16 <> compileCastOp AST.SExt Type.i32
compileInstr (S.I32Load16U (S.MemArg _ algn)) = compileLoadOp algn Type.i16 <> compileCastOp AST.ZExt Type.i32
compileInstr (S.I64Load8S  (S.MemArg _ algn)) = compileLoadOp algn Type.i8  <> compileCastOp AST.SExt Type.i64
compileInstr (S.I64Load8U  (S.MemArg _ algn)) = compileLoadOp algn Type.i8  <> compileCastOp AST.ZExt Type.i64
compileInstr (S.I64Load16S (S.MemArg _ algn)) = compileLoadOp algn Type.i16 <> compileCastOp AST.SExt Type.i64
compileInstr (S.I64Load16U (S.MemArg _ algn)) = compileLoadOp algn Type.i16 <> compileCastOp AST.ZExt Type.i64
compileInstr (S.I64Load32S (S.MemArg _ algn)) = compileLoadOp algn Type.i32 <> compileCastOp AST.SExt Type.i64
compileInstr (S.I64Load32U (S.MemArg _ algn)) = compileLoadOp algn Type.i32 <> compileCastOp AST.ZExt Type.i64

compileInstr (S.I32Store   (S.MemArg _ algn)) = compileStoreOp algn Type.i32
compileInstr (S.I64Store   (S.MemArg _ algn)) = compileStoreOp algn Type.i64
compileInstr (S.F32Store   (S.MemArg _ algn)) = compileStoreOp algn Type.float
compileInstr (S.F64Store   (S.MemArg _ algn)) = compileStoreOp algn Type.double
-- not sure how to wrap integers in LLVM
-- compileInstr (S.I32Store8  (S.MemArg _ algn)) = compileStoreOp algn Type.i8
-- compileInstr (S.I32Store16 (S.MemArg _ algn)) = compileStoreOp algn Type.i16
-- compileInstr (S.I64Store8  (S.MemArg _ algn)) = compileStoreOp algn Type.i8
-- compileInstr (S.I64Store16 (S.MemArg _ algn)) = compileStoreOp algn Type.i16
-- compileInstr (S.I64Store32 (S.MemArg _ algn)) = compileStoreOp algn Type.i32

--LLVM: ??
--compileInstr S.CurrentMemory = compileStoreOp S.BS64 boomwi 
--LLVM: Alloca
-- compileInstr S.GrowMemory = compileMemGrow


compileInstr instr = throwError $ "not implemented: " ++ show instr

-- newLoadInstr :: Type.Type -> InstrGen (AST.Instruction -> AST.Named AST.Instruction)
-- newLoadInstr Type.VoidType = pure AST.Do
-- newLoadInstr idType        = do
--   name <- gets $ makeName "tmp" . localIdentifier
--   let identifier = AST.ConstantOperand idType name
--   pushOperand identifier
--   incrLocalIdentifier
--   pure (name AST.:=)

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

compileRetTypeList :: [S.ValueType] -> Type.Type
compileRetTypeList []  = Type.VoidType
compileRetTypeList [t] = compileType t
compileRetTypeList l   = Type.StructureType True $ compileType <$> l

-- compiles one WASM function into an LLVM function.
compileFunction :: Name.Name -> S.Function -> ModGen Global.Global
compileFunction ident func = evalInstrGen instrGen initExprST
  where
    initExprST = InstrST M.empty 0 0 ident [] M.empty M.empty M.empty

    assignName (n, (t, instrs)) = (makeName "block" n, t, instrs)
    -- split the list of `LLVMObj` into blocks by looking for terminators. The
    -- terminators and the code in the corresponding blocks are then
    -- separated into an 'association list.'
    -- FIXME: splitAfter is not good enough since some blocks are nested under
    -- `if` in WASM (and possibly other instructions). These need to be
    -- un-nested into their own blocks in LLVM.
    instrGen    = do
      tell ["func def: " ++ show ident]
      ModEnv { startFunctionIndex, functionTypes } <- ask
      -- compile function type information
      let FT { arguments, returnType } = functionTypes M.! ident
          paramList  = do
            (i, t) <- zip [0..] arguments
            pure $ Global.Parameter t (makeName "local" i) []
          parameters = (paramList, False)
          -- if indx == startIndex then "main" else "func{indx}". All the extra code
          -- handle the fact that startIndex may or may not have a value
          name = maybe ident (const "main")
            $ guard . ((==ident) . makeName "func") =<< startFunctionIndex
          localTypes = compileType <$> S.localTypes func
          localVariables = do
            (n, t) <- zip [0..] $ arguments ++ localTypes
            pure (makeName "local" n, t)
      modify $ \st -> st { localVariableTypes = M.fromAscList localVariables }
      -- compile basic blocks and collect the results
      instrs              <- foldMap compileInstr $ Language.Wasm.Structure.body func
      llvmInstrs          <- renameInstrs instrs
      (phis, returnInstr) <- returnOperandStackItems
      let blks = splitAfter isTerm
               $ appendIfLast (not . isRet) (T returnInstr)
               $ llvmInstrs ++ fmap I phis
          basicBlocks = fmap (buildBlock . assignName)
                      $ zip [0..]
                      $ fromMaybe (error "empty block")
                      $ traverse unsnoc blks
      pure $ Global.functionDefaults { Global.basicBlocks
                                     , Global.name
                                     , Global.returnType
                                     , Global.parameters }

    buildBlock (name, term, instrs) = AST.BasicBlock name (unwrapI <$> instrs) (unwrapT term)

    isRet (T (AST.Do (AST.Ret _ _))) = True
    isRet _                              = False

    unwrapT (T t) = t
    unwrapT instr = error $ "Not an LLVM terminator: " ++ show instr

    unwrapI (I i) = i
    unwrapI term  = error $ "Not an LLVM instruction: " ++ show term

    -- rename instructions to reflect "variable" assignments
    renameInstrs = traverse renameInstr
      where
        renameInstr :: LLVMInstr -> InstrGen LLVMInstr
        renameInstr (I ((AST.:=) name instr)) = gets $ I . flip (AST.:=) instr . rename name
        renameInstr t@(I _)                   = pure t
        renameInstr t@(T _)                   = pure t
        rename name = fromMaybe name . M.lookup name . renameMap

-- FIXME: use linear memory
-- globals in the WASM sense of the term
compileGlobals :: Natural -> S.Global -> ModGen AST.Global
compileGlobals index global = undefined
  -- where
  --   globVar     = do
  --     init' <- init
  --     let initializer = Just init'
  --     pure $ AST.globalVariableDefaults { Global.name
  --                                       , Global.isConstant
  --                                       , Global.type'
  --                                       , Global.initializer
  --                                       }

  --   name       = makeName "global" index
  --   isConstant = isConst $ S.globalType global
  --   type'      = compileType' $ S.globalType global
  --   exp        = foldMap compileInstr $ S.initializer global
  --   -- TODO: maybe there's a way around this? The wasm specs says
  --   -- the initializer should be `expr` which could really be
  --   -- anything. Not sure how to implement that into LLVM though.
  --   init       = do l                          <- exp
  --                   guard $ null l
  --                   (_, AST.ConstantOperand c) <- popOperand
  --                   pure c


  --   isConst (S.Const _) = True
  --   isConst _           = False

  --   compileType' (S.Const t) = compileType t
  --   compileType' (S.Mut   t) = compileType t

-- AST.moduleDefinitions Module
-- ASTElem = Type.StructureType false [Type.IntegerType, Type.FunctionType]
compileMemory :: Natural -> S.Memory -> ModGen AST.Global
compileMemory index memory = globVar
  where
    globVar     = do
      pure $ AST.globalVariableDefaults { Global.name
                                        , Global.isConstant 
                                        , Global.type'
                                        }

    name       = makeName "Memory" index
    isConstant = True
    type'      = Type.NamedTypeReference "Memory"

compileTable :: Natural -> S.Table -> ModGen AST.Global
compileTable index table = globVar
  where
    globVar     = do
      pure $ AST.globalVariableDefaults { Global.name
                                        , Global.linkage
                                        -- , Global.isConstant 
                                        , Global.type'
                                        , Global.visibility
                                        , Global.alignment
                                        }
    -- currently: @table = external   constant %Tabl
    name       = Name.mkName "table"
    linkage    = Linkage.Internal
    visibility = Visibility.Default
    isConstant = True
    type'      = Type.NamedTypeReference "Table"
    alignment = 8
    
compileElement :: S.ElemSegment -> [AST.Instruction]
compileElement element = instrs
  where 
  -- load table???
    instrs = uncurry store' <$> zip [0..] (S.funcIndexes element)
  -- store item
      where
        store' fidx idx = AST.Store False address ref2 Nothing 8 []
          where 
            opConstr t name = Operand.ConstantOperand $ Constant.GlobalReference t name
            address = opConstr (Type.NamedTypeReference "Elem") $ Name.mkName "table"
            ref2    = opConstr (AST.FunctionType Type.i32 [] False) (makeName "func" idx)
  

-- assume they're all for same table
compileElements :: [S.ElemSegment] -> ModGen Global.Global
compileElements elements = initTable
  where
    initTable = do
      pure $ Global.functionDefaults { Global.name
                                     , Global.returnType
                                     , Global.basicBlocks 
                                     }
    name = Name.mkName "tableInit"
    -- allocate memory for table
    -- add elements to table; for each element in funcIndexes: [1,1,0..]
    instrs = concatMap compileElement elements
    -- instrs = [AST.Alloca Type.i32 (Just (AST.ConstantOperand $ Constant.Int 32 1)) 4 []]
    basicBlocks = [AST.BasicBlock "tableInit" (assignName instrs) (AST.Do $ AST.Ret Nothing [])]
    assignName instrs = fmap ("12" AST.:=) instrs
    -- basicBlocks = fmap (buildBlock . assignName)
    --         $ zip [0..]
    --         $ fromMaybe (error "empty block")
    --         $ traverse unsnoc blks
    -- buildBlock (name, term, instrs) = AST.BasicBlock name instrs term
    -- unwrapI (I i) = i
    -- unwrapI term  = error $ "Not an LLVM instruction: " ++ show term
    returnType = Type.void 
    linkage    = Linkage.Internal
    finalInstrs = []
    totalElems :: [S.ElemSegment] -> Int -> Int
    totalElems [] n = n
    totalElems (a:x) n = totalElems x (n+y)
      where
        y = length (S.funcIndexes a)


--compileImports :: Natural -> S.Import -> ModGen AST.Global
--compileImports index i = importItem
--  where importItem = do
--       pure $ Global.functionDefaults { Global.basicBlocks,
--                                         }

-- TODO: tables, elems
-- TODO: mems

--  t.load memarg and t.loadN_sx memarg

--  1. Let F be the current frame.
--  2. Assert: due to validation, F.module.memaddrs[0] exists.
--  3. Let a be the memory address F.module.memaddrs[0].
--  4. Assert: due to validation, S.mems[a] exists.
--  5. Let mem be the memory instance S.mems[a].
--  6. Assert: due to validation, a value of value type i32 is on the top of the stack.
--  7. Pop the value i32.const i from the stack.
--  8. Let ea be the integer i+memarg.offset.
--  9. If N is not part of the instruction, then:
--  a. Let N be the bit width |t| of value type t.
--  10. If ea+N/8 is larger than the length of mem.data, then:
--  a. Trap.
--  11. Let b∗ be the byte sequence mem.data[ea:N/8].
--  12. If N and sx are part of the instruction, then:
--  a. Let n be the integer for which bytesiN(n)=b∗.
--  b. Let c be the result of computing extend_sxN,|t|(n).
--  13. Else:
--  a. Let c be the constant for which bytest(c)=b∗.
--  14. Push the value t.const c to the stack.

-- load addr
-- local.set $x

-- %tmp0 = load ...
-- %..   = add i32 ... %tmp0

compileLoadOp :: Natural -> Type.Type -> InstrGen [LLVMInstr]
compileLoadOp alignment valType = do
  (phi, addr) <- popOperand
  let algn     = fromIntegral alignment
      ptrType  = Type.ptr valType
  memRef      <- asks memoryReference
  getEltPtr   <- newNamedInstruction ptrType $ AST.GetElementPtr True memRef [addr] []
  (_, ptr)    <- popOperand  -- no phi
  loadInstr   <- newNamedInstruction valType $ AST.Load False ptr Nothing algn []
  let instrs = phi ++ [getEltPtr, loadInstr]
  tell $ toLog "    emit: " instrs
  pure $ fmap I instrs

newNamedInstruction :: Type.Type -> AST.Instruction -> InstrGen (AST.Named AST.Instruction)
newNamedInstruction t instr = newInstructionConstructor t <*> pure instr

compileStoreOp :: Natural -> Type.Type -> InstrGen [LLVMInstr]
compileStoreOp alignment valType = do
  (phiV, val)  <- popOperand
  (phiA, addr) <- popOperand
  let algn     = fromIntegral alignment
      ptrType  = Type.ptr valType
  memRef       <- asks memoryReference
  getEltPtr    <- newNamedInstruction ptrType $ AST.GetElementPtr True memRef [addr] []
  (_, ptr)     <- popOperand
  let storeInstr = AST.Do $ AST.Store False ptr val Nothing algn []
      instrs     = phiV ++ phiA ++ [getEltPtr, storeInstr]
  tell $ toLog "    emit: " instrs
  pure $ fmap I instrs

-- compileMemGrow :: InstrGen [LLVMInstr]
-- compileMemGrow  =  do
--   (phiV, val)  <- popOperand
--   let instrs = [llvmMalloc ]
--   -- tell $ toLog "    emit: " instrs
--   -- pure $ fmap I instrs

-- TODO: datas
-- TODO: imports
-- TODO: exports
compileModule :: S.Module -> Either String AST.Module
compileModule wasmMod = evalModGen modGen initModEnv
  where
    startFunctionIndex  = (\(S.StartFunction n) -> n) <$> S.start wasmMod
    extractFuncType     = compileFunctionType . (S.types wasmMod !!) . fromIntegral . S.funcType
    functionTypes       = M.fromList
                        $ zip [makeName "func" i | i <- [0..]] 
                        $ extractFuncType <$> S.functions wasmMod
    functionTypeIndices = M.fromList $ zip [0..] $ compileFunctionType <$> S.types wasmMod
    ref name typeName   = AST.ConstantOperand 
                        $ flip Constant.GlobalReference (Name.mkName name)
                        $ Type.ptr
                        $ Type.NamedTypeReference typeName
    memoryReference     = ref "memory" "memory"
    tableReference      = ref "table" "table"
    initModEnv          = ModEnv { startFunctionIndex
                                 , functionTypes
                                 , functionTypeIndices
                                 , memoryReference
                                 , tableReference
                                 }
    wasmGlobals         = zip [0..] $ S.globals wasmMod
    wasmFuncs           = zip [makeName "func" i | i <- [0..]] $ S.functions wasmMod
    wasmElements        = S.elems wasmMod
    (minSize, maxSize)  = (\(S.Memory (S.Limit n x)) -> (n, x)) $ head $ S.mems wasmMod
--  wasmImports         = zip [0..] $ S.imports wasmMod

    modGen              = do
      -- set up table
      let elemNames    = [makeName "func" i | S.ElemSegment _ _ index <- wasmElements, i <- index]
          tableType    = do func <- elemNames
                            let FT { arguments, returnType } = functionTypes M.! func
                            pure $ Type.ptr $ Type.FunctionType returnType arguments False
          tableRefType = AST.StructureType False tableType
          tableTypeDef = AST.TypeDefinition "table" $ Just tableRefType
          tableElems   = uncurry Constant.GlobalReference <$> zip tableType elemNames
          tableInit    = Constant.Struct Nothing False tableElems
          table        = AST.globalVariableDefaults { Global.name = "table"
                                                    , Global.isConstant = True
                                                    , Global.type' = Type.NamedTypeReference $ Name.mkName "table"
                                                    , Global.initializer = Just tableInit
                                                    }

      -- set up memory
      let memoryType    = Type.ptr $ Type.i8
      let memoryTypeDef = AST.TypeDefinition "memory" $ Just memoryType
      let memory        = AST.globalVariableDefaults { Global.name = "memory"
                                                     , Global.type' = Type.NamedTypeReference $ Name.mkName "memory"
                                                     , Global.initializer = Nothing
                                                     }

      globals  <- traverse (uncurry compileGlobals) wasmGlobals
      -- imports <- traverse (uncurry compileImports) wasmImports
      funcs    <- traverse (uncurry compileFunction) wasmFuncs

      let globalDefs = AST.GlobalDefinition <$> table : memory : llvmIntrinsics ++ funcs
          typeDefs   = [tableTypeDef, memoryTypeDef]

      pure $ AST.defaultModule { AST.moduleName = "basic"
                               , AST.moduleDefinitions = typeDefs ++ globalDefs
                               }

parseModule :: B.ByteString -> Either String S.Module
parseModule = Wasm.parse
