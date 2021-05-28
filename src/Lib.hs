{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Applicative ((<|>))
import Control.Monad.Reader
import Control.Monad.State.Lazy
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
import qualified LLVM.AST.CallingConvention as Conv
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Float as Float
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Type as Type

import Intrinsics
import Gen
import Numeric.Natural
import Utils (appendIfLast, makeName, splitAfter, unsnoc, operandType)
import qualified LLVM.AST as AST.Instruction

type BOI    = Bool -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BOOI   = Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BBOOI  = Bool -> Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type FOOI   = AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type POOI p = p -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type OOI    = AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type MOI    = Maybe AST.Operand -> AST.InstructionMetadata -> AST.Terminator
type BOMWI = Bool -> AST.Operand -> Maybe AST.Atomicity -> Word32 -> AST.InstructionMetadata -> AST.Instruction
type BOOMWI = Bool -> AST.Operand -> AST.Operand -> Maybe AST.Atomicity -> Word32 -> AST.InstructionMetadata -> AST.Instruction

type UnOpBuilder a = a -> AST.Operand -> AST.Instruction
type BinOpBuilder a = a -> AST.Operand -> AST.Operand -> AST.Instruction
type CmpBuilder p = POOI p -> p -> AST.Operand -> AST.Operand -> AST.Instruction
type LoadMemBuilder = AST.Operand -> Word32 -> AST.Instruction
type StoreMemBuilder = AST.Operand -> AST.Operand -> Word32 -> AST.Instruction


bomwi :: LoadMemBuilder
bomwi addr aln = AST.Load False addr Nothing aln []

boomwi :: StoreMemBuilder
boomwi addr val aln = AST.Store False addr val Nothing aln []

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
wasmIsTerm (S.If _ _ _)  = True
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

compileConst :: Constant.Constant -> FuncGen [LLVMInstr]
compileConst const = (pushOperand $ AST.ConstantOperand const) $> []

compileBinOp :: BinOpBuilder a -> a -> Type.Type -> FuncGen [LLVMInstr]
compileBinOp builder op t = do
  (phiB, b)   <- popOperand
  (phiA, a)   <- popOperand
  -- generate a new identifier for the intermediate result. In LLVM IR
  -- this amounts to saving the results to a 'variable.'
  constructor <- newInstructionConstructor t
  pure $ fmap I $ phiB ++ phiA ++ [constructor $ builder op a b]

compileUnOp :: UnOpBuilder a -> a -> Type.Type -> FuncGen [LLVMInstr]
compileUnOp builder op t = do
  (instr, a)  <- popOperand
  constructor <- newInstructionConstructor t
  pure $ fmap I $ instr ++ [constructor $ builder op a]

compileCmpOp :: POOI p -> p -> FuncGen [LLVMInstr]
compileCmpOp cmp pred = do
  (phiB, b)   <- popOperand
  (phiA, a)   <- popOperand
  -- comparison operators return i1 sized booleans
  constructor <- newInstructionConstructor Type.i1
  pure $ fmap I $ phiB ++ phiA ++ [constructor $ pooi cmp pred a b]

compileFunctionCall :: Name.Name -> [Type.Type] -> Type.Type -> FuncGen [LLVMInstr]
compileFunctionCall name arguments returnType = do
  let nArgs                        = fromIntegral (L.length arguments)
      function                     = AST.ConstantOperand
                                   $ flip Constant.GlobalReference name 
                                   $ Type.FunctionType returnType arguments False
  -- pop the required number of operands off the `operandStack` and
  -- collect the results into a list. `replicateM` deals with the
  -- FuncGen monad.
  ops       <- replicateM (fromIntegral nArgs) popOperand
  let (phis, args) = unzip ops
      arguments    = reverse [(operand, []) | operand <- args]
      instr        = AST.Call Nothing Conv.C [] (Right function) arguments [] []
  constructor <- newInstructionConstructor returnType
  pure $ fmap I $ concat phis ++ [constructor instr]

callIntrinsics :: String -> FuncGen [LLVMInstr]
callIntrinsics name = do
  let FT { arguments, returnType } = llvmIntrinsicsTypes M.! name
  compileFunctionCall (Name.mkName name) arguments returnType

compileInstr :: S.Instruction Natural -> FuncGen [LLVMInstr]
compileInstr S.Unreachable          = pure [T $ AST.Do $ AST.Unreachable []]
compileInstr S.Nop                  = pure []

compileInstr (S.IUnOp bs S.IClz)    = pushOperand false *> callIntrinsics ("llvm.ctlz.i" ++ sBitSize bs)
compileInstr (S.IUnOp bs S.ICtz)    = pushOperand false *> callIntrinsics ("llvm.cttz.i" ++ sBitSize bs)
compileInstr (S.IUnOp bs S.IPopcnt) = callIntrinsics $ "llvm.ctpop.i" ++ sBitSize bs

compileInstr (S.FUnOp bs S.FAbs)     = callIntrinsics $ "llvm.fabs.f" ++ sBitSize bs
compileInstr (S.FUnOp S.BS32 S.FNeg) = compileInstr (S.F32Const (-1)) *> compileInstr (S.FBinOp S.BS32 S.FMul)
compileInstr (S.FUnOp S.BS64 S.FNeg) = compileInstr (S.F64Const (-1)) *> compileInstr (S.FBinOp S.BS64 S.FMul)
compileInstr (S.FUnOp bs S.FCeil)    = callIntrinsics $ "llvm.ceil.f" ++ sBitSize bs
compileInstr (S.FUnOp bs S.FFloor)   = callIntrinsics $ "llvm.floor.f" ++ sBitSize bs
compileInstr (S.FUnOp bs S.FTrunc)   = callIntrinsics $ "llvm.trunc.f" ++ sBitSize bs
compileInstr (S.FUnOp bs S.FNearest) = callIntrinsics $ "llvm.roundeven.f" ++ sBitSize bs
compileInstr (S.FUnOp bs S.FSqrt)    = callIntrinsics $ "llvm.sqrt.f" ++ sBitSize bs

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

compileInstr (S.IBinOp bs S.IAdd)  = compileBinOp bbooi AST.Add  $ iBitSize bs
compileInstr (S.IBinOp bs S.ISub)  = compileBinOp bbooi AST.Sub  $ iBitSize bs
compileInstr (S.IBinOp bs S.IMul)  = compileBinOp bbooi AST.Mul  $ iBitSize bs
compileInstr (S.IBinOp bs S.IDivS) = compileBinOp booi  AST.SDiv $ iBitSize bs
compileInstr (S.IBinOp bs S.IRemS) = compileBinOp ooi   AST.SRem $ iBitSize bs
compileInstr (S.IBinOp bs S.IAnd)  = compileBinOp ooi   AST.And  $ iBitSize bs
compileInstr (S.IBinOp bs S.IOr)   = compileBinOp ooi   AST.Or   $ iBitSize bs
compileInstr (S.IBinOp bs S.IXor)  = compileBinOp ooi   AST.Xor  $ iBitSize bs
compileInstr (S.IBinOp bs S.IShl)  = compileBinOp bbooi AST.Shl  $ iBitSize bs

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
  pure $ fmap I $ phiC ++ phiF ++ phiT ++ [constructor $ AST.Select cond true false []]

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
  (phi, a) <- popOperand
  case a of
    (AST.LocalReference _ name) -> addRenameAction name ident
    (AST.ConstantOperand const) -> assignConstant ident const
    _                           -> error "unsupported operand"
  pure $ I <$> phi

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
      let outofblockInstr = do
            constructor <- newInstructionConstructor varType
            let operand = first AST.ConstantOperand . swap <$> M.assocs constants
            pure [I $ constructor $ AST.Phi varType operand []]
          inblockInstr c  = pushOperand (AST.ConstantOperand c) $> []
          blockID         = makeName "block" blockIdentifier
      maybe outofblockInstr inblockInstr $ M.lookup blockID constants

compileInstr (S.Call i) = do
  ModEnv { functionTypes } <- ask
  let FT { arguments, returnType } = functionTypes M.! i
      name                         = makeName "func" i
  compileFunctionCall name arguments returnType

compileInstr S.Return = do
  (phi, term) <- returnOperandStackItems
  blockID     <- makeName "block" . blockIdentifier <$> get
  deleteOperandStack blockID
  pure $ (I <$> phi) ++ [T term]

compileInstr (S.If _ b1 b2) = do
  blockIndx <- blockIdentifier <$> get
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
  b1Compiled      <- concat <$> traverse compileInstr (appendTerm b1)
  _               <- moveOperandStack originID b2Ident  -- same instrs as thenInstrs
  b2Compiled      <- concat <$> traverse compileInstr (appendTerm b2)
  popScope
  let instr = T $ AST.Do $ AST.CondBr operand b1Ident b2Ident []
  pure $ fmap I phiP ++ fmap I thenInstrs ++ [instr] ++ b1Compiled ++ b2Compiled

-- the index here is the levels of scopes. It's a WASM peculiarity
compileInstr (S.Br i)       = do
  origin   <- makeName "block" . blockIdentifier <$> get
  dest     <- readScope $ fromIntegral i
  brInstrs <- moveOperandStack origin dest
  incrBlockIdentifier
  pure $ fmap I brInstrs ++ [T $ AST.Do $ AST.Br dest []]

compileInstr (S.BrIf i)     = do
  blockIndx       <- blockIdentifier <$> get
  dest            <- readScope $ fromIntegral i
  (phiP, operand) <- popOperand
  let origin      = makeName "block" blockIndx
      fallthrough = makeName "block" $ blockIndx + 1
  brInstrs        <- branchOperandStack origin dest
  _               <- moveOperandStack origin fallthrough  -- same instrs as thenInstrs
  incrBlockIdentifier
  pure $ fmap I phiP ++ fmap I brInstrs ++ [T $ AST.Do $ AST.CondBr operand dest fallthrough []]

compileInstr (S.Block _ body) = do
  blockIndx <- blockIdentifier <$> get
  let wasmInstrs     = appendIfLast (not . wasmIsTerm) (S.Br 0) body
      numberOfBlocks = countTerminals wasmInstrs
      endOfBlock     = makeName "block" $ blockIndx + numberOfBlocks
  pushScope endOfBlock
  compiled <- concat <$> traverse compileInstr wasmInstrs
  popScope
  pure compiled

-- the difference with Block is where the scope starts
compileInstr (S.Loop _ body) = do
  blockIndx <- blockIdentifier <$> get
  let wasmInstrs     = appendIfLast (not . wasmIsTerm) (S.Br 1) body
      numberOfBlocks = countTerminals wasmInstrs
      startOfBlock   = makeName "block" $ blockIndx + 1
      endOfBlock     = makeName "block" $ blockIndx + numberOfBlocks + 1
      start          = T $ AST.Do $ AST.Br startOfBlock []
  pushScope endOfBlock
  pushScope startOfBlock
  compiled <- concat <$> traverse compileInstr wasmInstrs
  popScope
  popScope
  pure $ start : compiled

compileInstr (S.I32Load memArg) = compileMemInstr S.BS32 bomwi memArg
compileInstr (S.I64Load memArg) = compileMemInstr S.BS64 bomwi memArg
compileInstr (S.F32Load memArg) = compileMemInstr S.BS32 bomwi memArg
compileInstr (S.F64Load memArg) = compileMemInstr S.BS64 bomwi memArg
-- compileInstr (S.I32Load8S  S.MemArArBS) = compileMemInstr S.BS32 bomwi memArg
compileInstr (S.I32Load8U  memArg) = compileMemInstr S.BS32 bomwi memArg
-- compileInstr (S.I32Load16S  S.MemArg) = compileMemInstr S.BS32 bomwi memArg
-- compileInstr (S.I32Load16U  S.MemArg) = compileMemInstr S.BS32 bomwi memArg
-- compileInstr (S.I64Load8S  S.MemArg) = compileMemInstr S.BS64 bomwi memArg
-- compileInstr (S.I64Load8U  S.MemArg) = compileMemInstr S.BS64 bomwi memArg
-- compileInstr (S.I64Load16S  S.MemArg) = compileMemInstr S.BS64 bomwi memArg
-- compileInstr (S.I64Load16U  S.MemArg) = compileMemInstr S.BS64 bomwi memArg
-- compileInstr (S.I64Load32S  S.MemArg) = compileMemInstr S.BS64 bomwi memArg
-- compileInstr (S.I64Load32U  memArg) = compileMemInstr S.BS64 bomwi memArg


compileInstr (S.I32Store memArg) = compileMem2Instr S.BS32 boomwi memArg
compileInstr (S.I64Store memArg) = compileMem2Instr S.BS64 boomwi memArg
-- compileInstr (S.F32Store S.MemArg) = compileMem2Instr S.BS32 boomwi memArg
-- compileInstr (S.F64Store S.MemArg) = compileMem2Instr S.BS64 boomwi memArg
-- compileInstr (S.I32Store8 S.MemArg) = compileMem2Instr S.BS32 boomwi memArg
-- compileInstr (S.I32Store16 S.MemArg) = compileMem2Instr S.BS32 boomwi memArg
-- compileInstr (S.I64Store8 S.MemArg) = compileMem2Instr S.BS64 boomwi memArg
-- compileInstr (S.I64Store16 S.MemArg) = compileMem2Instr S.BS64 boomwi memArg
-- compileInstr (S.I64Store32 S.MemArg) = compileMem2Instr S.BS64 boomwi memArg

compileInstr instr = error $ "not implemented: " ++ show instr


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
compileFunction :: Natural -> S.Function -> ModGen Global.Global
compileFunction indx func = evalFuncGen funcGen initFuncST
  where
    initFuncST = FuncST M.empty 0 0 [] M.empty M.empty M.empty

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
      modify $ \st -> st { localVariableTypes = M.fromAscList localVariables }
      -- compile basic blocks and collect the results
      instrs              <- fmap concat $ traverse compileInstr $ Language.Wasm.Structure.body func
      llvmInstrs          <- renameInstrs instrs
      (phis, returnInstr) <- returnOperandStackItems
      let blks = splitAfter isTerm
               $ appendIfLast (not . isRet) (T returnInstr)
               $ llvmInstrs ++ fmap I phis
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

    -- rename instructions to reflect "variable" assignments
    renameInstrs = traverse renameInstr
      where
        renameInstr :: LLVMInstr -> FuncGen LLVMInstr
        renameInstr (I ((AST.:=) name instr)) = I . flip (AST.:=) instr . rename name <$> get
        renameInstr t@(I _)                   = pure t
        renameInstr t@(T _)                   = pure t
        rename name = fromMaybe name . M.lookup name . renameMap

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

compileMemory :: Natural -> S.Table -> ModGen AST.Global
compileMemory index table = globVar
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
                                        , Global.isConstant 
                                        , Global.type'
                                        }

    name       = makeName "table" index
    isConstant = True
    type'      = Type.NamedTypeReference "Table"
    
-- compileElement :: Natural -> S.ElemSegment -> [AST.Instruction]
-- compileElement index element = do
--   let offset = AST.Alloca Type.i32 1 4 []
--   [offset]

-- compileElements :: Natural -> [S.ElemSegment] -> ModGen Global.Global
-- compileElements index elements = do
  



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

compileMemInstr :: S.BitSize -> LoadMemBuilder -> S.MemArg -> FuncGen [LLVMInstr]
compileMemInstr bs builder memArg = --addr algn 
  do
    (phi, addr) <- popOperand
    let algn = fromIntegral $ S.align memArg

    -- generate a new identifier for the intermediate result. In LLVM IR
    -- this amounts to saving the results to a 'variable.'
    
    constructor <- newInstructionConstructor $ iBitSize bs
    pure $ fmap I $ phi ++ [constructor $ builder addr algn]

compileMem2Instr :: S.BitSize -> StoreMemBuilder -> S.MemArg -> FuncGen [LLVMInstr]
compileMem2Instr bs builder memArg = do
  (phiV, val)  <- popOperand
  (phiA, addr) <- popOperand
  let algn = fromIntegral $ S.align memArg
  pure $ fmap I $ phiV ++ phiA ++ [AST.Do $ builder addr val algn]


-- TODO: datas
-- TODO: imports
-- TODO: exports
compileModule :: S.Module -> Either String AST.Module
compileModule wasmMod = evalModGen modGen initModEnv
  where
    startFunctionIndex  = (\(S.StartFunction n) -> n) <$> S.start wasmMod
    functionTypes       = M.fromList $ zip [0..] $ compileFunctionType <$> S.types wasmMod
    initModEnv          = ModEnv startFunctionIndex functionTypes
    wasmGlobals         = zip [0..] $ S.globals wasmMod
    wasmTables          = zip [0..] $ S.tables wasmMod
    wasmFuncs           = zip [0..] $ S.functions wasmMod
    modGen              = do
      globals <- traverse (uncurry compileGlobals) wasmGlobals
      tables  <- traverse (uncurry compileTable) wasmTables
      defs    <- traverse (uncurry compileFunction) wasmFuncs
      pure $ AST.defaultModule
        { AST.moduleName = "basic",
          AST.moduleDefinitions = 
            (AST.GlobalDefinition <$> (llvmIntrinsics ++ defs)) ++
            (AST.GlobalDefinition <$> tables) ++
            [
              AST.TypeDefinition "Elem" (Just (AST.StructureType False [Type.i32, Type.ptr (Type.FunctionType Type.void [] False)]))
            , AST.TypeDefinition "Table" (Just (AST.StructureType False [Type.ptr (Type.NamedTypeReference "Elem"), Type.i32, Type.i32]))
            , AST.TypeDefinition "Memory" (Just (AST.StructureType False [Type.ptr Type.i8, Type.i32, Type.i32, Type.i32]))
            ] 
        }

parseModule :: B.ByteString -> Either String S.Module
parseModule = Wasm.parse
