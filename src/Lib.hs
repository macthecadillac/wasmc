{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.Word
import Data.Tuple
import Debug.Trace
import Numeric.Natural
import System.Info (arch)

import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Structure as Struct
import qualified Language.Wasm.Structure (Function(..))

import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as Addr
import qualified LLVM.AST.CallingConvention as Conv
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Float as Float
import qualified LLVM.AST as AST.Instruction
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Type as Type
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Visibility as Visibility

import External (externalFunctions, externalFunctionTypes, true, false)
import Gen
import Utils (appendIfLast, makeName, splitAfter, unsnoc, operandType, toLog, machineNativeWordWidth)

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

isTerm :: LLVMInstr -> Bool
isTerm (T _) = True
isTerm _     = False

sBitSize :: Struct.BitSize -> String
sBitSize Struct.BS32 = "32"
sBitSize Struct.BS64 = "64"

nBitSize :: Struct.BitSize -> Word32
nBitSize Struct.BS32 = 32
nBitSize Struct.BS64 = 64

iBitSize :: Struct.BitSize -> Type.Type
iBitSize Struct.BS32 = Type.i32
iBitSize Struct.BS64 = Type.i64

fBitSize :: Struct.BitSize -> Type.Type
fBitSize Struct.BS32 = Type.float
fBitSize Struct.BS64 = Type.double

-- check if a wasm instruction will be compiled to a terminator in LLVM
wasmIsTerm :: Struct.Instruction a -> Bool
wasmIsTerm Struct.Unreachable = True
wasmIsTerm Struct.Return      = True
wasmIsTerm Struct.If {}       = True
wasmIsTerm (Struct.Br _)      = True
wasmIsTerm (Struct.BrIf _)    = True
wasmIsTerm (Struct.Block _ _) = True
wasmIsTerm (Struct.Loop _ _)  = True
wasmIsTerm _             = False

countTerminals :: [Struct.Instruction Natural] -> Natural
countTerminals = sum . fmap aux
  where
    aux (Struct.If _ [] []) = 2
    aux (Struct.If _ [] b2) = 1 + oneIfTailIsNotTerm b2 + countTerminals b2
    aux (Struct.If _ b1 []) = 1 + oneIfTailIsNotTerm b1 + countTerminals b1
    aux (Struct.If _ b1 b2) = oneIfTailIsNotTerm b1 + oneIfTailIsNotTerm b2 + countTerminals b1 + countTerminals b2
    aux (Struct.Block _ []) = 1
    aux (Struct.Block _ l)  = oneIfTailIsNotTerm l + countTerminals l
    aux (Struct.Br _)       = 1
    aux (Struct.BrIf _)     = 1
    aux (Struct.Loop _ [])  = 1
    aux (Struct.Loop _ l)   = 1 + oneIfTailIsNotTerm l + countTerminals l
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
  (phiB, b)  <- popOperand
  (phiA, a)  <- popOperand
  -- generate a new identifier for the intermediate result. In LLVM IR
  -- this amounts to saving the results to a 'variable.'
  binOpInstr <- newNamedInstruction t $ builder op a b
  let instrs = phiB ++ phiA ++ [I binOpInstr]
  tell $ toLog "    emit: " instrs
  pure instrs

compileUnOp :: UnOpBuilder a -> a -> Type.Type -> InstrGen [LLVMInstr]
compileUnOp builder op t = do
  (phi, a)  <- popOperand
  unOpInstr <- newNamedInstruction t $ builder op a
  let instrs = phi ++ [I unOpInstr]
  tell $ toLog "    emit: " instrs
  pure instrs

compileCmpOp :: POOI p -> p -> InstrGen [LLVMInstr]
compileCmpOp cmp pred = do
  (phiB, b)  <- popOperand
  (phiA, a)  <- popOperand
  -- comparison operators return i1 sized booleans
  cmpOpInstr <- newNamedInstruction Type.i1 $ pooi cmp pred a b
  -- TODO: output of wasm cmp ops are i32 whereas in llvm it is i1. It is of
  -- course preferable to directly go to i1 but for simplicity and for type
  -- sanity we cast back to i32 before moving forward
  bitcast    <- castIntIfNotBitsize 32
  let instrs = phiB ++ phiA ++ [I cmpOpInstr] ++ bitcast
  tell $ toLog "    emit: " instrs
  pure instrs
  
compileCastOp :: OTI -> Type.Type -> InstrGen [LLVMInstr]
compileCastOp op t = do
  (phi, a)    <- popOperand
  castOpInstr <- newNamedInstruction t $ op a t []
  let instrs = phi ++ [I castOpInstr]
  tell $ toLog "    emit: " instrs
  pure instrs

compileFunctionCall :: AST.Operand -> [Type.Type] -> Type.Type -> InstrGen [LLVMInstr]
compileFunctionCall function args = aux
  where
    -- `<>` is the "summation" operator over the semigroup. It's a
    -- generalization of `++` for lists and `+`/`*` for numbers
    aux retType@Type.StructureType { Type.elementTypes } = alloc <> singleRet <> pushStack
      where
        ptrTy  = Type.ptr retType -- default addr

        alloc = do
          allocInstr <- newNamedInstruction ptrTy $ AST.Alloca ptrTy Nothing 0 []
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
                ptrInstr  <- newNamedInstruction ptrType unamedGetPtr
                (_, addr) <- popOperand  -- no phi
                loadInstr <- newNamedInstruction t $ AST.Load False addr Nothing 0 []
                pure [ptrInstr, loadInstr]
          pushStackInstrs <- foldMap genInstr $ zip [0..] elementTypes
          tell $ toLog "               " pushStackInstrs
          pure $ fmap I pushStackInstrs

    aux retType = singleReturnCall args retType

    singleReturnCall arguments returnType = do
      let nArgs    = fromIntegral (L.length arguments)
      -- pop the required number of operands off the `operandStack` and
      -- collect the results into a list. `replicateM` deals with the
      -- InstrGen monad.
      ops        <- replicateM (fromIntegral nArgs) popOperand
      let (phis, args) = unzip ops
          arguments    = reverse [(operand, []) | operand <- args]
          instr        = AST.Call Nothing Conv.C [] (Right function) arguments [] []
      namedInstr <- newNamedInstruction returnType instr
      pure $ concat phis ++ [I namedInstr]

callExternal :: String -> InstrGen [LLVMInstr]
callExternal name = do
  let FT { arguments, returnType } = externalFunctionTypes M.! name
      name'    = Name.mkName name
      function = AST.ConstantOperand
               $ flip Constant.GlobalReference name'
               $ Type.FunctionType returnType arguments False
  modify $ \st -> st { externalFuncs = S.insert name' $ externalFuncs st }
  compileFunctionCall function arguments returnType

intToPtr :: Type.Type -> InstrGen [LLVMInstr]
intToPtr ptrType = do
  (phi, ptr) <- popOperand
  instr      <- newNamedInstruction ptrType $ AST.IntToPtr ptr ptrType []
  pure $ phi ++ [I instr]

ptrToInt :: Type.Type -> InstrGen [LLVMInstr]
ptrToInt intType = do
  (phi, ptr) <- popOperand
  instr      <- newNamedInstruction intType $ AST.PtrToInt ptr intType []
  pure $ phi ++ [I instr]

getEltPtr :: Type.Type -> [AST.Operand] -> InstrGen [LLVMInstr]
getEltPtr ptrType indices = do
  (phi, ptr) <- popOperand
  instr      <- newNamedInstruction ptrType $ AST.GetElementPtr True ptr indices []
  pure $ phi ++ [I instr]

castBit :: Type.Type -> InstrGen [LLVMInstr]
castBit ptrType = do
  (phi, ptr) <- popOperand  -- no phi
  castPtr    <- newNamedInstruction ptrType $ AST.BitCast ptr ptrType []
  pure $ phi ++ [I castPtr]

load :: Type.Type -> InstrGen [LLVMInstr]
load valType = do
  (phi, ptr) <- popOperand
  loadInstr  <- newNamedInstruction valType $ AST.Load False ptr Nothing 0 []
  pure $ phi ++ [I loadInstr]

store :: AST.Operand -> InstrGen [LLVMInstr]
store val = do
  (phi, ptr) <- popOperand
  let instr = I $ AST.Do $ AST.Store False ptr val Nothing 0 []
  pure $ phi ++ [instr]

-- load from linear memory
compileLoadOp :: Type.Type -> InstrGen [LLVMInstr]
compileLoadOp valType = do
  (phi, addr) <- popOperand
  let ptrType    = Type.ptr valType
      memPtrType = Type.ptr Type.i8
  memIntRef   <- asks memoryReference
  isize       <- isizeM
  pushOperand memIntRef
  instrs <- load isize
         <> intToPtr memPtrType
         <> getEltPtr memPtrType [addr]
         <> castBit ptrType
         <> load valType
  tell $ toLog "    emit: " $ phi ++ instrs
  pure $ phi ++ instrs

-- store to linear memory
compileStoreOp :: Type.Type -> InstrGen [LLVMInstr]
compileStoreOp valType = do
  (phiV, val)  <- popOperand
  (phiA, addr) <- popOperand
  let ptrType    = Type.ptr valType
      memPtrType = Type.ptr Type.i8
  isize        <- isizeM
  memIntRef    <- asks memoryReference
  pushOperand memIntRef
  instrs <- load isize
         <> intToPtr memPtrType
         <> getEltPtr memPtrType [addr]
         <> castBit ptrType
         <> store val
  tell $ toLog "    emit: " $ phiV ++ phiA ++ instrs
  pure $ phiV ++ phiA ++ instrs

compileInstr :: Struct.Instruction Natural -> InstrGen [LLVMInstr]
compileInstr Struct.Unreachable          = pure [T $ AST.Do $ AST.Unreachable []]
compileInstr Struct.Nop                  = pure []
compileInstr Struct.Drop                 = popOperand $> []

compileInstr (Struct.IUnOp bs Struct.IClz)    = pushOperand false *> callExternal ("llvm.ctlz.i" ++ sBitSize bs)
compileInstr (Struct.IUnOp bs Struct.ICtz)    = pushOperand false *> callExternal ("llvm.cttz.i" ++ sBitSize bs)
compileInstr (Struct.IUnOp bs Struct.IPopcnt) = callExternal $ "llvm.ctpop.i" ++ sBitSize bs

compileInstr (Struct.FUnOp bs Struct.FAbs)              = callExternal $ "llvm.fabs.f" ++ sBitSize bs
compileInstr (Struct.FUnOp Struct.BS32 Struct.FNeg)     = foldMap compileInstr [Struct.F32Const (-1), Struct.FBinOp Struct.BS32 Struct.FMul]
compileInstr (Struct.FUnOp Struct.BS64 Struct.FNeg)     = foldMap compileInstr [Struct.F64Const (-1), Struct.FBinOp Struct.BS64 Struct.FMul]
compileInstr (Struct.FUnOp bs Struct.FCeil)             = callExternal $ "llvm.ceil.f" ++ sBitSize bs
compileInstr (Struct.FUnOp bs Struct.FFloor)            = callExternal $ "llvm.floor.f" ++ sBitSize bs
compileInstr (Struct.FUnOp bs Struct.FTrunc)            = callExternal $ "llvm.trunc.f" ++ sBitSize bs
compileInstr (Struct.FUnOp Struct.BS32 Struct.FNearest) = callExternal "roundevenf"
compileInstr (Struct.FUnOp Struct.BS64 Struct.FNearest) = callExternal "roundeven"
compileInstr (Struct.FUnOp bs Struct.FSqrt)             = callExternal $ "llvm.sqrt.f" ++ sBitSize bs

compileInstr Struct.I32Eqz                = foldMap compileInstr [Struct.I32Const 0, Struct.IRelOp Struct.BS32 Struct.IEq]
compileInstr Struct.I64Eqz                = foldMap compileInstr [Struct.I64Const 0, Struct.IRelOp Struct.BS64 Struct.IEq]

-- not sure how to wrap integers in LLVM
-- compileInstr Struct.I32WrapI64            = throwError "not implemented: Struct.I32WrapI64"
compileInstr Struct.I64ExtendSI32         = compileCastOp AST.SExt Type.i64
compileInstr Struct.I64ExtendUI32         = compileCastOp AST.ZExt Type.i64
compileInstr (Struct.ITruncFU _ bs)       = compileCastOp AST.SIToFP $ iBitSize bs
compileInstr (Struct.ITruncFS _ bs)       = compileCastOp AST.UIToFP $ iBitSize bs
compileInstr (Struct.FConvertIU _ bs)     = compileCastOp AST.FPToUI $ iBitSize bs
compileInstr (Struct.FConvertIS _ bs)     = compileCastOp AST.FPToSI $ iBitSize bs
compileInstr Struct.F32DemoteF64          = compileCastOp AST.FPTrunc Type.float
compileInstr Struct.F64PromoteF32         = compileCastOp AST.FPExt Type.double
compileInstr (Struct.IReinterpretF bs)    = compileCastOp AST.BitCast $ iBitSize bs
compileInstr (Struct.FReinterpretI bs)    = compileCastOp AST.BitCast $ iBitSize bs

compileInstr (Struct.IBinOp bs Struct.IAdd)  = compileBinOp bbooi AST.Add  $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.ISub)  = compileBinOp bbooi AST.Sub  $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IMul)  = compileBinOp bbooi AST.Mul  $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IDivS) = compileBinOp booi  AST.SDiv $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IDivU) = compileBinOp booi  AST.UDiv $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IRemS) = compileBinOp ooi   AST.SRem $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IAnd)  = compileBinOp ooi   AST.And  $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IOr)   = compileBinOp ooi   AST.Or   $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IXor)  = compileBinOp ooi   AST.Xor  $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IShl)  = compileBinOp bbooi AST.Shl  $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IShrU) = compileBinOp booi  AST.LShr $ iBitSize bs
compileInstr (Struct.IBinOp bs Struct.IShrS) = compileBinOp booi  AST.AShr $ iBitSize bs

compileInstr (Struct.IRelOp _  Struct.IEq)   = compileCmpOp AST.ICmp IPred.EQ
compileInstr (Struct.IRelOp _  Struct.INe)   = compileCmpOp AST.ICmp IPred.NE
compileInstr (Struct.IRelOp _  Struct.ILtU)  = compileCmpOp AST.ICmp IPred.ULT
compileInstr (Struct.IRelOp _  Struct.ILtS)  = compileCmpOp AST.ICmp IPred.SLT
compileInstr (Struct.IRelOp _  Struct.IGtU)  = compileCmpOp AST.ICmp IPred.UGT
compileInstr (Struct.IRelOp _  Struct.IGtS)  = compileCmpOp AST.ICmp IPred.SGT
compileInstr (Struct.IRelOp _  Struct.ILeU)  = compileCmpOp AST.ICmp IPred.ULE
compileInstr (Struct.IRelOp _  Struct.ILeS)  = compileCmpOp AST.ICmp IPred.SLE
compileInstr (Struct.IRelOp _  Struct.IGeU)  = compileCmpOp AST.ICmp IPred.UGE
compileInstr (Struct.IRelOp _  Struct.IGeS)  = compileCmpOp AST.ICmp IPred.SGE

compileInstr (Struct.FBinOp bs Struct.FAdd)  = compileBinOp fooi AST.FAdd $ fBitSize bs
compileInstr (Struct.FBinOp bs Struct.FSub)  = compileBinOp fooi AST.FSub $ fBitSize bs
compileInstr (Struct.FBinOp bs Struct.FMul)  = compileBinOp fooi AST.FMul $ fBitSize bs
compileInstr (Struct.FBinOp bs Struct.FDiv)  = compileBinOp fooi AST.FDiv $ fBitSize bs
compileInstr (Struct.FBinOp bs Struct.FMin)  = callExternal $ "llvm.minnum.f" ++ sBitSize bs
compileInstr (Struct.FBinOp bs Struct.FMax)  = callExternal $ "llvm.maxnum.f" ++ sBitSize bs
-- value of the first argument, sign of the second argument
compileInstr (Struct.FBinOp bs Struct.FCopySign) = callExternal $ "llvm.copysign.f" ++ sBitSize bs

compileInstr (Struct.FRelOp _  Struct.FEq)   = compileCmpOp AST.FCmp FPred.OEQ
compileInstr (Struct.FRelOp _  Struct.FNe)   = compileCmpOp AST.FCmp FPred.ONE
compileInstr (Struct.FRelOp _  Struct.FLt)   = compileCmpOp AST.FCmp FPred.OLT
compileInstr (Struct.FRelOp _  Struct.FGt)   = compileCmpOp AST.FCmp FPred.OGT
compileInstr (Struct.FRelOp _  Struct.FLe)   = compileCmpOp AST.FCmp FPred.OLE
compileInstr (Struct.FRelOp _  Struct.FGe)   = compileCmpOp AST.FCmp FPred.OGE

compileInstr Struct.Select              = do
  (phiC, cond)  <- popOperand
  (phiF, false) <- popOperand
  (phiT, true)  <- popOperand
  let opType = operandType true
  selectInstr   <- newNamedInstruction opType $ AST.Select cond true false []
  let instrs = phiC ++ phiF ++ phiT ++ [I selectInstr]
  tell $ toLog "    emit: " instrs
  pure instrs

compileInstr (Struct.I32Const n)        = compileConst $ Constant.Int 32 $ fromIntegral n
compileInstr (Struct.I64Const n)        = compileConst $ Constant.Int 64 $ fromIntegral n
compileInstr (Struct.F32Const n)        = compileConst $ Constant.Float $ Float.Single n
compileInstr (Struct.F64Const n)        = compileConst $ Constant.Float $ Float.Double n

compileInstr (Struct.SetGlobal n)       = throwError "not implemented: SetGlobal"
compileInstr (Struct.TeeLocal n)        = do
  (phi, a) <- peekOperand
  let name = makeName "local" n
  varType  <- gets $ flip (M.!) name . localVariableTypes
  let ref   = AST.LocalReference (Type.ptr varType) name
      instr = phi ++ [I $ AST.Do $ AST.Store False ref a Nothing 0 []]
  tell $ toLog "    emit: " instr
  pure instr

compileInstr (Struct.SetLocal n)        = foldMap compileInstr [Struct.TeeLocal n, Struct.Drop]
compileInstr (Struct.GetGlobal n)       = throwError "not implemented: GetGlobal"

compileInstr (Struct.GetLocal n)        = do
  let name = makeName "local" n
  varType <- gets $ flip (M.!) name . localVariableTypes
  let ref = AST.LocalReference (Type.ptr varType) name
  instr   <- newNamedInstruction varType $ AST.Load False ref Nothing 0 []
  tell $ toLog "    emit: " [I instr]
  pure [I instr]

compileInstr (Struct.Call i) = do
  ModEnv { functionTypes } <- ask
  let FT { arguments, returnType } = functionTypes M.! makeName "func" i
      name                         = makeName "func" i
      function                     = AST.ConstantOperand
                                   $ flip Constant.GlobalReference name
                                   $ Type.FunctionType returnType arguments False
  compileFunctionCall function arguments returnType

compileInstr (Struct.CallIndirect i) = do
  funcTypeMap                  <- asks functionTypeIndices
  FT { arguments, returnType } <- liftMaybe $ M.lookup i funcTypeMap
  tableRef                     <- asks tableReference
  let zero        = AST.ConstantOperand $ Constant.Int 32 0
      funcPtrType = Type.ptr $ Type.FunctionType returnType arguments False
  isize       <- isizeM
  (phiI, idx) <- popOperand
  pushOperand tableRef
  ptrInstrs   <- getEltPtr (Type.ptr isize) [zero, idx]
              <> load isize
              <> intToPtr funcPtrType
  (_, func)   <- popOperand
  callInstrs  <- compileFunctionCall func arguments returnType
  pure $ ptrInstrs ++ callInstrs
    where
      liftMaybe = maybe (throwError "not a known function type") pure

compileInstr Struct.Return = do
  (phi, term) <- returnOperandStackItems
  blockID     <- gets $ makeName "block" . blockIdentifier
  deleteOperandStack blockID
  incrBlockIdentifier
  tell $ toLog "    return-emit: " phi
  tell $ toLog "    return-term: " [term]
  pure $ phi ++ [T term]

compileInstr (Struct.If _ b1 b2) = do
  blockIndx <- gets blockIdentifier
  let b1Count    = countTerminals b1
      b2Count    = countTerminals b2
      originID   = makeName "block" blockIndx
      b1Ident    = makeName "block" $ blockIndx + 1
      b2Ident    = makeName "block" $ blockIndx + b1Count + 2
      endIf      = makeName "block" $ blockIndx + b1Count + b2Count + 3
      appendTerm = appendIfLast (not . wasmIsTerm) (Struct.Br 0)
  pushOperand $ AST.ConstantOperand $ Constant.Int 32 0
  neqz            <- compileInstr (Struct.IRelOp Struct.BS32 Struct.INe)
  bitcast         <- castIntIfNotBitsize 1
  (_, operand)    <- popOperand
  incrBlockIdentifier
  pushScope endIf
  thenInstrs      <- branchOperandStack originID b1Ident
  b1Compiled      <- foldMap compileInstr $ appendTerm b1
  _               <- moveOperandStack originID b2Ident  -- same instrs as thenInstrs
  b2Compiled      <- foldMap compileInstr $ appendTerm b2
  popScope
  let term = T $ AST.Do $ AST.CondBr operand b1Ident b2Ident []
  tell $ toLog "    if-emit: " $ bitcast ++ thenInstrs
  tell $ toLog "    if-term: " [term]
  pure $ neqz ++ bitcast ++ thenInstrs ++ [term] ++ b1Compiled ++ b2Compiled

-- the index here is the levels of scopes. It's a WASM peculiarity
compileInstr (Struct.Br i)       = do
  origin   <- gets $ makeName "block" . blockIdentifier
  dest     <- readScope $ fromIntegral i
  brInstrs <- moveOperandStack origin dest
  incrBlockIdentifier
  let term = AST.Do $ AST.Br dest []
  tell $ toLog "    br-emit: " brInstrs
  tell $ toLog "    br-term: " [term]
  pure $ brInstrs ++ [T term]

compileInstr (Struct.BrIf i)     = do
  blockIndx       <- gets blockIdentifier
  dest            <- readScope $ fromIntegral i
  pushOperand $ AST.ConstantOperand $ Constant.Int 32 0
  neqz            <- compileInstr (Struct.IRelOp Struct.BS32 Struct.INe)
  bitcast         <- castIntIfNotBitsize 1
  (_, operand)    <- popOperand
  let origin      = makeName "block" blockIndx
      fallthrough = makeName "block" $ blockIndx + 1
  brInstrs        <- branchOperandStack origin dest
  _               <- moveOperandStack origin fallthrough
  incrBlockIdentifier
  let term = AST.Do $ AST.CondBr operand dest fallthrough []
  tell $ toLog "    brif-emit: " $ bitcast ++ brInstrs
  tell $ toLog "    brif-term: " [term]
  pure $ neqz ++ bitcast ++ brInstrs ++ [T term]

compileInstr (Struct.Block _ body) = do
  tell ["  begin block"]
  blockIndx <- gets blockIdentifier
  let wasmInstrs     = appendIfLast (not . wasmIsTerm) (Struct.Br 0) body
      numberOfBlocks = countTerminals wasmInstrs
      endOfBlock     = makeName "block" $ blockIndx + numberOfBlocks
  pushScope endOfBlock
  compiled <- foldMap compileInstr wasmInstrs
  popScope
  tell ["  end block"]
  pure compiled

-- the difference with Block is where the scope starts
compileInstr (Struct.Loop _ body) = do
  tell ["  begin loop"]
  origin       <- gets $ makeName "block" . blockIdentifier
  incrBlockIdentifier
  startOfBlock <- gets $ makeName "block" . blockIdentifier
  let wasmInstrs     = appendIfLast (not . wasmIsTerm) (Struct.Br 1) body
      start          = T $ AST.Do $ AST.Br startOfBlock []
  pushScope startOfBlock
  brInstrs   <- branchOperandStack origin startOfBlock
  compiled   <- foldMap compileInstr wasmInstrs
  let nextBlock = gets (makeName "block" . (+1) . blockIdentifier)
  endOfBlock <- readScope 1 <|> nextBlock
  _          <- moveOperandStack origin (trace (show endOfBlock) endOfBlock)
  _          <- moveOperandStack startOfBlock endOfBlock
  popScope
  tell ["  end loop"]
  pure $ brInstrs ++ [start] ++ compiled

compileInstr (Struct.I32Load  _) = compileLoadOp Type.i32
compileInstr (Struct.I64Load  _) = compileLoadOp Type.i64
compileInstr (Struct.F32Load  _) = compileLoadOp Type.float
compileInstr (Struct.F64Load  _) = compileLoadOp Type.double
-- load one byte as i8, then sign extend to i32.
compileInstr (Struct.I32Load8S  _) = compileLoadOp Type.i8  <> compileCastOp AST.SExt Type.i32
compileInstr (Struct.I32Load8U  _) = compileLoadOp Type.i8  <> compileCastOp AST.ZExt Type.i32
compileInstr (Struct.I32Load16S _) = compileLoadOp Type.i16 <> compileCastOp AST.SExt Type.i32
compileInstr (Struct.I32Load16U _) = compileLoadOp Type.i16 <> compileCastOp AST.ZExt Type.i32
compileInstr (Struct.I64Load8S  _) = compileLoadOp Type.i8  <> compileCastOp AST.SExt Type.i64
compileInstr (Struct.I64Load8U  _) = compileLoadOp Type.i8  <> compileCastOp AST.ZExt Type.i64
compileInstr (Struct.I64Load16S _) = compileLoadOp Type.i16 <> compileCastOp AST.SExt Type.i64
compileInstr (Struct.I64Load16U _) = compileLoadOp Type.i16 <> compileCastOp AST.ZExt Type.i64
compileInstr (Struct.I64Load32S _) = compileLoadOp Type.i32 <> compileCastOp AST.SExt Type.i64
compileInstr (Struct.I64Load32U _) = compileLoadOp Type.i32 <> compileCastOp AST.ZExt Type.i64

compileInstr (Struct.I32Store  _) = compileStoreOp Type.i32
compileInstr (Struct.I64Store  _) = compileStoreOp Type.i64
compileInstr (Struct.F32Store  _) = compileStoreOp Type.float
compileInstr (Struct.F64Store  _) = compileStoreOp Type.double
-- not sure how to wrap integers in LLVM
-- compileInstr (Struct.I32Store8  (Struct.MemArg _ algn)) = compileStoreOp algn Type.i8
-- compileInstr (Struct.I32Store16 (Struct.MemArg _ algn)) = compileStoreOp algn Type.i16
-- compileInstr (Struct.I64Store8  (Struct.MemArg _ algn)) = compileStoreOp algn Type.i8
-- compileInstr (Struct.I64Store16 (Struct.MemArg _ algn)) = compileStoreOp algn Type.i16
-- compileInstr (Struct.I64Store32 (Struct.MemArg _ algn)) = compileStoreOp algn Type.i32

--LLVM: ??
--compileInstr Struct.CurrentMemory = compileStoreOp Struct.BS64 boomwi 
--LLVM: Alloca
-- compileInstr Struct.GrowMemory = compileMemGrow


compileInstr instr = throwError $ "not implemented: " ++ show instr

castIntIfNotBitsize :: Word32 -> InstrGen [LLVMInstr]
castIntIfNotBitsize bs = upcast <|> downcast <|> mempty
  where
    operandBS (Operand.LocalReference (Type.IntegerType bs) _) = bs
    operandBS (Operand.ConstantOperand (Constant.Int bs _))    = bs
    operandBS _                                                = error "not an integer"

    targetType   = Type.IntegerType bs
    upcast       = cast (>) AST.ZExt
    downcast     = cast (<) AST.Trunc
    cast pred op = do
      (_, bool) <- popOperand  -- no phi
      let bs' = operandBS bool
      guard $ pred bs bs'
      bitcast   <- newNamedInstruction targetType $ op bool targetType []
      pure [I bitcast]

compileType :: Struct.ValueType -> Type.Type
compileType Struct.I32 = Type.IntegerType 32
compileType Struct.I64 = Type.IntegerType 64
compileType Struct.F32 = Type.FloatingPointType Type.FloatFP
compileType Struct.F64 = Type.FloatingPointType Type.DoubleFP

compileFunctionType :: Struct.FuncType -> FunctionType
compileFunctionType Struct.FuncType { Struct.params, Struct.results } = FT arguments returnType
  where
    arguments  = compileType <$> params
    returnType = compileRetTypeList results

compileRetTypeList :: [Struct.ValueType] -> Type.Type
compileRetTypeList []  = Type.VoidType
compileRetTypeList [t] = compileType t
compileRetTypeList l   = Type.StructureType True $ compileType <$> l

isizeM :: (MonadReader ModEnv m) => m Type.Type
isizeM = asks $ Type.IntegerType . nativeWordWidth

-- compiles one WASM function into an LLVM function.
compileFunction :: Name.Name -> Struct.Function -> ModGen (S.Set Name.Name, [Global.Global])
compileFunction ident func = evalInstrGen instrGen initExprST
  where
    initExprST = InstrST M.empty 0 0 ident [] M.empty S.empty

    assignName (n, (t, instrs)) = (makeName "block" n, t, instrs)

    isStart func = Global.name func == Name.mkName "_main"

    initMem = do
      guard $ ident == "_main"
      minSize   <- asks memoryMinSize
      memRef    <- asks memoryReference
      isize     <- isizeM
      size      <- sizeT $ minSize * 64000
      _         <- compileInstr size
      malloc    <- callExternal "malloc"
      castInstr <- ptrToInt isize
      (_, int)  <- popOperand
      let storeInstr = AST.Do $ AST.Store False memRef int Nothing 0 []
      pure $ malloc ++ castInstr ++ [I storeInstr]

    dropMem = do
      guard $ ident == "_main"
      isize   <- isizeM
      memRef  <- asks memoryReference
      pushOperand memRef
      load isize <> intToPtr (Type.ptr Type.i8) <> callExternal "free"

    initTbl = do
      guard $ ident == "_main"
      tblElems    <- asks tableElements
      foldMap (uncurry funcRefToInt) $ M.toList tblElems

    funcRefToInt index name = do
      FT { arguments, returnType } <- asks $ flip (M.!) name . functionTypes
      let funcRef = AST.ConstantOperand
                  $ flip Constant.GlobalReference name
                  $ Type.ptr
                  $ Type.FunctionType returnType arguments False
          zero    = AST.ConstantOperand $ Constant.Int 32 0
          offset  = AST.ConstantOperand $ Constant.Int 32 (fromIntegral index)
      isize           <- isizeM
      tblPtr          <- asks tableReference
      tblRefType      <- asks tableRefType
      castInstr       <- newNamedInstruction isize $ AST.PtrToInt funcRef isize []
      (_, funcPtrInt) <- popOperand
      getEltPtr       <- newNamedInstruction (Type.ptr isize) $ AST.GetElementPtr True tblPtr [zero, offset] []
      (_, tblEltPtr)  <- popOperand
      let storePtr = AST.Do $ AST.Store False tblEltPtr funcPtrInt Nothing 0 []
      pure [I castInstr, I getEltPtr, I storePtr]

    allocStack = do
      argTypes <- asks $ arguments . flip (M.!) ident . functionTypes
      let localTypes = compileType <$> Struct.localTypes func
      pure $ foldMap alloc
           $ zip3 [0..] (argTypes ++ localTypes)
           $ fmap (const True) argTypes ++ repeat False

    alloc (i, t, isArg) | isArg     = allocInstr ++ initWithArg
                        | otherwise = allocInstr
      where
        initWithArg = [I $ AST.Do $ AST.Store False ident arg Nothing 0 []]
        allocInstr  = [I $ makeName "local" i AST.:= AST.Alloca t Nothing 0 []]
        ident       = AST.LocalReference (Type.ptr t) name
        arg         = AST.LocalReference t $ makeName "arg" i
        name        = makeName "local" i

    sizeT int = case arch of
                   "x86_64" -> pure $ Struct.I64Const $ fromIntegral int
                   "i386"   -> pure $ Struct.I32Const $ fromIntegral int
                   "ia64"   -> pure $ Struct.I64Const $ fromIntegral int
                   "arm"    -> pure $ Struct.I32Const $ fromIntegral int
                   _        -> throwError "not a supported architecture"

    instrGen    = do
      tell ["func def: " ++ show ident]
      -- initialize memory if it is the main function
      memInitInstrs                <- initMem <|> mempty
      tblInitInstrs                <- initTbl <|> mempty
      dropInstrs                   <- dropMem <|> mempty
      initStack                    <- allocStack
      -- compile function type information
      FT { arguments, returnType } <- asks $ flip (M.!) ident . functionTypes
      let paramList  = do
            (i, t) <- zip [0..] arguments
            pure $ Global.Parameter t (makeName "arg" i) []
          parameters = (paramList, False)
          localTypes = compileType <$> Struct.localTypes func
          localVariables = do
            (n, t) <- zip [0..] $ arguments ++ localTypes
            pure (makeName "local" n, t)
      modify $ \st -> st { localVariableTypes = M.fromAscList localVariables }
      -- compile basic blocks and collect the results
      llvmInstrs          <- foldMap compileInstr $ Language.Wasm.Structure.body func
      (phis, returnInstr) <- returnOperandStackItems
      externFs            <- gets externalFuncs
      let blks = splitAfter isTerm
               $ appendIfLast (not . isRet) (T returnInstr)
               $ memInitInstrs ++ tblInitInstrs ++ initStack ++ llvmInstrs ++ phis ++ dropInstrs
          basicBlocks = fmap (buildBlock . assignName)
                      $ zip [0..]
                      $ fromMaybe (error "empty block")
                      $ traverse unsnoc blks
          func = Global.functionDefaults { Global.basicBlocks
                                         , Global.name = ident
                                         , Global.returnType
                                         , Global.parameters }
      pure (externFs, [func])

    buildBlock (name, term, instrs) = AST.BasicBlock name (unwrapI <$> instrs) (unwrapT term)

    isRet (T (AST.Do (AST.Ret _ _))) = True
    isRet _                          = False

    unwrapT (T t) = t
    unwrapT instr = error $ "Not an LLVM terminator: " ++ show instr

    unwrapI (I i) = i
    unwrapI term  = error $ "Not an LLVM instruction: " ++ show term

-- FIXME: use linear memory
-- globals in the WASM sense of the term
processGlobals :: Natural -> Struct.Global -> ModGen AST.Global
processGlobals index global = undefined
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
  --   isConstant = isConst $ Struct.globalType global
  --   type'      = compileType' $ Struct.globalType global
  --   exp        = foldMap compileInstr $ Struct.initializer global
  --   -- TODO: maybe there's a way around this? The wasm specs says
  --   -- the initializer should be `expr` which could really be
  --   -- anything. Not sure how to implement that into LLVM though.
  --   init       = do l                          <- exp
  --                   guard $ null l
  --                   (_, AST.ConstantOperand c) <- popOperand
  --                   pure c


  --   isConst (Struct.Const _) = True
  --   isConst _           = False

  --   compileType' (Struct.Const t) = compileType t
  --   compileType' (Struct.Mut   t) = compileType t

--compileImports :: Natural -> Struct.Import -> ModGen AST.Global
--compileImports index i = importItem
--  where importItem = do
--       pure $ Global.functionDefaults { Global.basicBlocks,
--                                         }

-- compileMemGrow :: InstrGen [LLVMInstr]
-- compileMemGrow  = do
--   (phiV, val)  <- popOperand
--   let instrs = [llvmMalloc ]
--   -- tell $ toLog "    emit: " instrs
--   -- pure $ fmap I instrs

-- TODO: datas
-- TODO: imports
-- TODO: exports
compileModule :: Struct.Module -> Either String AST.Module
compileModule wasmMod = do
  wordWidth    <- machineNativeWordWidth
  let memoryReference = AST.ConstantOperand
                      $ flip Constant.GlobalReference (Name.mkName "wasmc.linear_mem")
                      $ Type.ptr
                      $ Type.IntegerType wordWidth

      -- set up table
      elemNames     = [makeName "func" i | Struct.ElemSegment _ _ index <- wasmElements, i <- index]
      tableElements = M.fromList $ zip [0..] elemNames
      nElems        = length elemNames
      isize         = Type.IntegerType wordWidth
      tableInit     = Constant.Array isize $ replicate nElems $ Constant.Int wordWidth 0
      tableType     = Type.ArrayType (fromIntegral nElems) isize
      tableTypeDef  = AST.TypeDefinition "wasmc.tbl" $ Just tableType
      tableRefType  = Type.ptr tableType
      tableGlobType = Type.NamedTypeReference $ Name.mkName "wasmc.tbl"
      table         = AST.globalVariableDefaults { Global.name = "wasmc.tbl"
                                                 , Global.type' = tableGlobType
                                                 , Global.initializer = Just tableInit
                                                 }

      -- set up memory
      memoryInit    = Just $ Constant.Int wordWidth $ fromIntegral (minSize * 64000)
      minMemorySize = AST.globalVariableDefaults { Global.name = "wasmc.min_mem_size"
                                                 , Global.isConstant = True
                                                 , Global.type' = Type.i64
                                                 , Global.initializer = memoryInit
                                                 }
      memory        = AST.globalVariableDefaults { Global.name = "wasmc.linear_mem"
                                                 , Global.type' = Type.IntegerType wordWidth
                                                 , Global.initializer = Just $ Constant.Int wordWidth 0
                                                 }

      modGen = do
        globals           <- traverse (uncurry processGlobals) wasmGlobals
        -- imports <- traverse (uncurry compileImports) wasmImports
        compilationOutput <- traverse (uncurry compileFunction) wasmFuncs
        let (decls, funcs) = mconcat compilationOutput
            externs        = filter (flip S.member decls . Global.name) externalFunctions

        let globalDefs = AST.GlobalDefinition <$> table : minMemorySize : memory : externs ++ funcs
            typeDefs   = [tableTypeDef]

        pure $ AST.defaultModule { AST.moduleName = "basic"
                                 , AST.moduleDefinitions = typeDefs ++ globalDefs
                                 }

      initModEnv      = ModEnv { startFunctionIndex
                               , functionTypes
                               , functionTypeIndices
                               , nativeWordWidth = wordWidth
                               , memoryReference
                               , memoryMinSize = minSize
                               , memoryMaxSize = maxSize
                               , tableReference
                               , tableElements
                               , tableRefType
                               }

  evalModGen modGen initModEnv
    where
      startFunctionIndex  = (\(Struct.StartFunction n) -> n) <$> Struct.start wasmMod
      extractFuncType     = compileFunctionType . (Struct.types wasmMod !!) . fromIntegral . Struct.funcType
      functionTypes       = M.fromList
                          $ zip funcNames
                          $ extractFuncType <$> Struct.functions wasmMod
      functionTypeIndices = M.fromList $ zip [0..] $ compileFunctionType <$> Struct.types wasmMod
      tableReference      = AST.ConstantOperand 
                          $ flip Constant.GlobalReference (Name.mkName "wasmc.tbl")
                          $ Type.ptr
                          $ Type.NamedTypeReference (Name.mkName "wasmc.tbl")
      (minSize, maxSize)  = maybe (0, Nothing) ((\(Struct.Memory (Struct.Limit n x)) -> (n, x)) . fst)
                          $ L.uncons
                          $ Struct.mems wasmMod
      wasmGlobals         = zip [0..] $ Struct.globals wasmMod
      wasmData            = Struct.datas wasmMod
      wasmFuncs           = zip funcNames $ Struct.functions wasmMod
      funcName i          = maybe (makeName "func" i) (const "_main")
                          $ guard . (==i) =<< startFunctionIndex
      funcNames           = funcName <$> [0..]
      wasmElements        = Struct.elems wasmMod

--  wasmImports         = zip [0..] $ Struct.imports wasmMod


parseModule :: B.ByteString -> Either String Struct.Module
parseModule = Wasm.parse
