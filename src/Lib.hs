{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Data.Word
import Debug.Trace
import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Structure as S
import qualified Language.Wasm.Structure (Function(..))
import qualified Language.Wasm.Lexer as X

-- LLVM Stuff
import LLVM.AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.IntegerPredicate as P
import LLVM.AST.Name
import LLVM.AST.Global
import LLVM.AST.Type

import Numeric.Natural
import Utils (newName, splitWhen)

data WasmModST = WasmModST { startFunctionIndex :: Maybe Natural
                           , currentFunction :: [BasicBlock]
                           , currentBasicBlockInstrs :: [Named Instruction]
                           , operandStack :: [Operand]
                           , currentIdentifierNumber :: Natural
                           , functionTypes :: M.Map Natural S.FuncType }
                           deriving (Show)

data LLVMInstr = B BinOp | U UnOp deriving (Show)

--           tag   bitsize/fp type   type  
data BinOp = BBOOI Word32            BBOOI
           | BOOI  Word32            BOOI
           | FOOI  FloatingPointType FOOI
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

instance Show LLVMTerm where
  show Lib.Ret = "Ret"

type BOI =  Bool -> Operand -> InstructionMetadata -> Instruction
type BOOI =  Bool -> Operand -> Operand -> InstructionMetadata -> Instruction
type BBOOI = Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> Instruction
type FOOI = FastMathFlags -> Operand -> Operand -> InstructionMetadata -> Instruction
type IOOI =  IntegerPredicate -> Operand -> Operand -> InstructionMetadata -> Instruction
type OOI = Operand -> Operand -> InstructionMetadata -> Instruction
type MOI = Maybe Operand -> InstructionMetadata -> Terminator

data LLVMObj = Instr LLVMInstr
             | Op Operand
             | Term LLVMTerm
             deriving (Show)

iop :: BBOOI -> Operand -> Operand -> Instruction
iop op a b = op False False a b []

fop :: FOOI -> Operand -> Operand -> Instruction
fop op a b = op noFastMathFlags a b []

isLLVMInstr :: LLVMObj -> Bool
isLLVMInstr (Instr _) = True
isLLVMInstr _         = False

isLLVMOp :: LLVMObj -> Bool
isLLVMOp (Op _) = True
isLLVMOp _      = False

isLLVMTerm :: LLVMObj -> Bool
isLLVMTerm (Term _) = True
isLLVMTerm _        = False

unwrapOp :: LLVMObj -> Operand 
unwrapOp (Op op) = op
unwrapOp _       = error "not an operand"

unwrapTerm :: LLVMObj -> LLVMTerm
unwrapTerm (Term t) = t
unwrapTerm _        = error "not a terminator"

compileInstr :: S.Instruction Natural -> LLVMObj
compileInstr (S.I32Const n) = Op $ ConstantOperand $ C.Int 32 $ fromIntegral n
compileInstr (S.I64Const n) = Op $ ConstantOperand $ C.Int 64 $ fromIntegral n
compileInstr (S.F32Const n) = Op $ ConstantOperand $ C.Float $ F.Single n
compileInstr (S.F64Const n) = Op $ ConstantOperand $ C.Float $ F.Double n

-- Binary Operations(IBinOPs)
-- TODO: should use helper functions instead of explicitly matching the bitsize
compileInstr (S.IBinOp S.BS32 S.IAdd)  = Instr $ B $ BBOOI 32 Add
compileInstr (S.IBinOp S.BS32 S.ISub)  = Instr $ B $ BBOOI 32 Sub 
compileInstr (S.IBinOp S.BS32 S.IMul)  = Instr $ B $ BBOOI 32 Mul
compileInstr (S.IBinOp S.BS32 S.IDivS) = Instr $ B $ BOOI  32 SDiv
compileInstr (S.IBinOp S.BS32 S.IRemS) = Instr $ B $ OOI   32 SRem
compileInstr (S.IBinOp S.BS32 S.IAnd)  = Instr $ B $ OOI   32 And 
compileInstr (S.IBinOp S.BS32 S.IOr)   = Instr $ B $ OOI   32 Or
compileInstr (S.IBinOp S.BS32 S.IXor)  = Instr $ B $ OOI   32 Xor
compileInstr (S.IBinOp S.BS32 S.IShl)  = Instr $ B $ BBOOI 32 Shl

compileInstr (S.IBinOp S.BS64 S.IAdd)  = Instr $ B $ BBOOI 64 Add
compileInstr (S.IBinOp S.BS64 S.ISub)  = Instr $ B $ BBOOI 64 Sub 
compileInstr (S.IBinOp S.BS64 S.IMul)  = Instr $ B $ BBOOI 64 Mul
compileInstr (S.IBinOp S.BS64 S.IDivS) = Instr $ B $ BOOI  64 SDiv
compileInstr (S.IBinOp S.BS64 S.IRemS) = Instr $ B $ OOI   64 SRem
compileInstr (S.IBinOp S.BS64 S.IAnd)  = Instr $ B $ OOI   64 And 
compileInstr (S.IBinOp S.BS64 S.IOr)   = Instr $ B $ OOI   64 Or
compileInstr (S.IBinOp S.BS64 S.IXor)  = Instr $ B $ OOI   64 Xor
compileInstr (S.IBinOp S.BS64 S.IShl)  = Instr $ B $ BBOOI 64 Shl
-- count leading zeros, trailing zeros, popped vals (IUnOPs)//////TO FIX
--compileInstr (S.IUnOp _ S.IClz) = Instr $ -- Clz
--compileInstr (S.IUnOp _ S.ICtz) = Instr $ -- Ctz
--compileInstr (S.IUnOp _ S.IPopcnt) = Instr $ -- Popcnt
-- Relational Operations(IRelOPs) IEq	 INe	 ILtU	 ILtS	 IGtU	 IGtS	 ILeU	 ILeS	 IGeU	 IGeS
compileInstr (S.IRelOp S.BS32 S.IEq)  = Instr $ B $ OOI 32 $ ICmp P.EQ
compileInstr (S.IRelOp S.BS32 S.INe)  = Instr $ B $ OOI 32 $ ICmp P.NE
compileInstr (S.IRelOp S.BS32 S.ILtU) = Instr $ B $ OOI 32 $ ICmp P.ULT
compileInstr (S.IRelOp S.BS32 S.ILtS) = Instr $ B $ OOI 32 $ ICmp P.SLT
compileInstr (S.IRelOp S.BS32 S.IGtU) = Instr $ B $ OOI 32 $ ICmp P.UGT
compileInstr (S.IRelOp S.BS32 S.IGtS) = Instr $ B $ OOI 32 $ ICmp P.SGT
compileInstr (S.IRelOp S.BS32 S.ILeU) = Instr $ B $ OOI 32 $ ICmp P.ULE
compileInstr (S.IRelOp S.BS32 S.ILeS) = Instr $ B $ OOI 32 $ ICmp P.SLE
compileInstr (S.IRelOp S.BS32 S.IGeU) = Instr $ B $ OOI 32 $ ICmp P.UGE
compileInstr (S.IRelOp S.BS32 S.IGeS) = Instr $ B $ OOI 32 $ ICmp P.SGE

compileInstr (S.IRelOp S.BS64 S.IEq)  = Instr $ B $ OOI 64 $ ICmp P.EQ
compileInstr (S.IRelOp S.BS64 S.INe)  = Instr $ B $ OOI 64 $ ICmp P.NE
compileInstr (S.IRelOp S.BS64 S.ILtU) = Instr $ B $ OOI 64 $ ICmp P.ULT
compileInstr (S.IRelOp S.BS64 S.ILtS) = Instr $ B $ OOI 64 $ ICmp P.SLT
compileInstr (S.IRelOp S.BS64 S.IGtU) = Instr $ B $ OOI 64 $ ICmp P.UGT
compileInstr (S.IRelOp S.BS64 S.IGtS) = Instr $ B $ OOI 64 $ ICmp P.SGT
compileInstr (S.IRelOp S.BS64 S.ILeU) = Instr $ B $ OOI 64 $ ICmp P.ULE
compileInstr (S.IRelOp S.BS64 S.ILeS) = Instr $ B $ OOI 64 $ ICmp P.SLE
compileInstr (S.IRelOp S.BS64 S.IGeU) = Instr $ B $ OOI 64 $ ICmp P.UGE
compileInstr (S.IRelOp S.BS64 S.IGeS) = Instr $ B $ OOI 64 $ ICmp P.SGE
-- FUn Operations(FUnOPs) FAbs	 FNeg	 FCeil	 FFloor	 FTrunc	 FNearest	 FSqrt
--compileInstr (S.FUnOp _ S.FAbs) = Instr $ IOOI Abs

-- Terminators (return, br etc)
compileInstr S.Return = Term Lib.Ret
compileInstr instr = error $ "Not implemented in compileInstr: " ++ show instr

compileType :: S.ValueType -> Type
compileType S.I32 = IntegerType 32
compileType S.I64 = IntegerType 64
compileType S.F32 = FloatingPointType FloatFP
compileType S.F64 = FloatingPointType DoubleFP
    
buildBasicBlock :: Name -> LLVMTerm -> [LLVMObj] -> ExceptT String (State WasmModST) BasicBlock
buildBasicBlock name term llvmObjs = BasicBlock name <$> llvmInstrs <*> makeTerminator term
  where
    chunks = splitWhen (\x -> isLLVMInstr x || isLLVMTerm x) llvmObjs
    llvmInstrs = L.concat <$> traverse munch chunks

    makeTerminator :: LLVMTerm -> ExceptT String (State WasmModST) (Named Terminator)
    makeTerminator Lib.Ret = do
      retVal <- stackToRetVal . operandStack <$> get
      modify (\env -> env { operandStack = [] })
      pure $ Do $ LLVM.AST.Ret (Just retVal) []

    stackToRetVal []  = LocalReference VoidType "void"
    stackToRetVal [x] = x
    stackToRetVal l   = error "add support for multiple return vals" -- TODO: support multiple return values

    -- b
    -- a
    -- sub
    -- sub a b vs sub b a
    -- doesnt the instr come after the ops in wasm?
    -- the order of a and b might need to be switched
    munch :: [LLVMObj] -> ExceptT String (State WasmModST) [Named Instruction]
    munch = munchBackwards . L.reverse
      where
        munchBackwards [] = pure []
        munchBackwards (Instr (B op):Op a:Op b:rest) = do
          putOperandsOnStack rest
          identifier <- identify $ B op
          pure [identifier := buildBinOp op a b]
        munchBackwards [Instr (B op), Op a] = do
          env        <- get
          (b, rest)  <- maybe (fail "not enough operands") pure $ L.uncons $ operandStack env
          put $ env { operandStack = rest }
          identifier <- identify $ B op
          pure [identifier := buildBinOp op a b]
        munchBackwards [Instr (B op)] = do
          env          <- get
          (a, b, rest) <- case operandStack env of
            a:b:rest -> pure (a, b, rest)
            _        -> fail "not enough operands"
          put $ env { operandStack = rest }
          identifier   <- identify $ B op
          pure [identifier := buildBinOp op a b]
        munchBackwards l
          | all isLLVMOp l = putOperandsOnStack l *> pure []
          | otherwise      = error $ "Instruction sequence not implemented: " ++ show l -- FIXME: complete me!!

    buildBinOp :: BinOp -> Operand -> Operand -> Instruction
    buildBinOp (BBOOI _ op) = iop op
    buildBinOp (FOOI  _ op) = fop op
    buildBinOp o          = error $ "Not implemented for binop type: " ++ show o

    identify (B (BBOOI bs _)) = newIdentifier $ IntegerType bs
    identify (B (BOOI  bs _)) = newIdentifier $ IntegerType bs
    identify (B (OOI   bs _)) = newIdentifier $ IntegerType bs
    identify _                = error "not implmented"

    newIdentifier :: Type -> ExceptT String (State WasmModST) Name
    newIdentifier idType = do
      WasmModST { currentIdentifierNumber, operandStack } <- get
      let name       = newName "ident" $ currentIdentifierNumber + 1
          identifier = LocalReference idType name
      modify (\env -> env {
         currentIdentifierNumber = currentIdentifierNumber + 1,
         operandStack = identifier : operandStack
        })
      pure name

    putOperandsOnStack :: [LLVMObj] -> ExceptT String (State WasmModST) ()
    putOperandsOnStack l | not $ all isLLVMOp l = error "Not all operands."
                         | otherwise            = modify f
                       where
                         f env = env { operandStack = newOps ++ operandStack env }
                         newOps = unwrapOp <$> l

compileFunction :: Natural -> S.Function -> ExceptT String (State WasmModST) Global
compileFunction indx func = do
  WasmModST { startFunctionIndex, operandStack, functionTypes } <- get
  let funcType   = functionTypes M.! (S.funcType func)
      returnType = compileRetType $ S.results funcType
      paramList  = buildParamList $ S.params funcType
      parameters = (paramList, False)
      blks       = splitTerm <$> splitWhen isLLVMTerm llvmObjs  -- FIXME: questionable criteria here
      namedBlks  = assignName <$> zip [0..] blks
      -- if indx == startIndex then "main" else "func{indx}". All the extra code
      -- handle the fact that startIndex may or may not have a value
      name = maybe (newName "func" indx) id
        $ const "main" <$> (guard . (==indx) =<< startFunctionIndex)
  modify (\env -> env { currentIdentifierNumber = fromIntegral $ L.length paramList })
  basicBlocks <- traverse (\(n, t, o) -> buildBasicBlock n t o) namedBlks
  pure $ functionDefaults { basicBlocks, name, returnType, parameters }
  where
    llvmObjs = compileInstr <$> Language.Wasm.Structure.body func

    assignName (n, (t, instrs)) = (newName "block" n, t, instrs)

    compileRetType []  = VoidType
    compileRetType [t] = compileType t
    compileRetType l   = StructureType True $ compileType <$> l

    buildParamList l = do
      (i, t) <- zip [0..] l
      pure $ Parameter (compileType t) (newName "ident" i) []

    splitTerm :: [LLVMObj] -> (LLVMTerm, [LLVMObj])
    splitTerm []            = error "empty block"  -- this would be a bug
    splitTerm (Term t:rest) = (t, L.reverse rest)
    splitTerm instrs        = (Lib.Ret, instrs)

-- TODO: tables
-- TODO: mems
-- TODO: globals
-- TODO: elems
-- TODO: datas
-- TODO: imports
-- TODO: exports
compileModule :: S.Module -> Either String Module
compileModule wasmMod = do
  globalDefs <- buildGlobalDefs wasmMod
  let moduleDefinitions = GlobalDefinition <$> globalDefs
  pure $ defaultModule
    { moduleName = "basic",
      moduleDefinitions
    }
  where
    startFunctionIndex = (\(S.StartFunction n) -> n) <$> S.start wasmMod
    initModST = WasmModST { startFunctionIndex
                          , currentFunction = []
                          , currentBasicBlockInstrs = []
                          , operandStack = []
                          , currentIdentifierNumber = 0
                          , functionTypes = M.fromList $ zip [0..] $ S.types wasmMod
                          }
    buildGlobalDefs = flip evalState initModST
                    . runExceptT
                    . traverse (uncurry compileFunction)
                    . zip [0..]
                    . S.functions

parseModule :: B.ByteString -> Either String S.Module
parseModule = Wasm.parse

-- int :: Type
-- int = IntegerType 32

-- defAdd :: Definition
-- defAdd = GlobalDefinition functionDefaults
--   { name = Name "add"
--   , parameters =
--       ( [ Parameter int (Name "a") []
--         , Parameter int (Name "b") [] ]
--       , False )
--   , returnType = int
--   , basicBlocks = [body]
--   }
--   where
--     body = BasicBlock
--         (Name "entry")
--         [ Name "result" :=
--             Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (LocalReference int (Name "a"))
--                 (LocalReference int (Name "b"))
--                 []]
--         (Do $ Ret (Just (LocalReference int (Name "result"))) [])

-- module_ :: Module
-- module_ = defaultModule
--   { moduleName = "basic"
--   , moduleDefinitions = [defAdd]
--   }
