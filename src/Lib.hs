{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Control.Monad.State.Lazy
import Control.Monad.Except
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Short.Internal (ShortByteString, toShort)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Structure as S
import qualified Language.Wasm.Structure (Function(..))
import qualified Language.Wasm.Lexer as X

-- LLVM Stuff
import LLVM.AST
import LLVM.AST.IntegerPredicate
import LLVM.AST.Name
import LLVM.AST.Global
import LLVM.AST.Type

import Numeric.Natural
import Utils

data WasmModST = WasmModST {
  currentFuncIndex :: Natural,
  currentFunction :: [BasicBlock],
  currentBasicBlockInstrs :: [Named Instruction],
  operandStack :: [Operand],
  currentIdentifierNumber :: Natural,
  functionTypes :: M.Map Natural S.FuncType
}

data LLVMInstr = B BinOp | U UnOp
data BinOp = BBOOI BBOOI | BOOI BOOI | FOOI FOOI | OOI OOI | IOOI IOOI
data UnOp = BOI BOI
type BOI =  Bool -> Operand -> InstructionMetadata -> Instruction
type BOOI =  Bool -> Operand -> Operand -> InstructionMetadata -> Instruction
type BBOOI = Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> Instruction
type FOOI = FastMathFlags -> Operand -> Operand -> InstructionMetadata -> Instruction
type IOOI =  IntegerPredicate -> Operand -> Operand -> InstructionMetadata -> Instruction
type OOI = Operand -> Operand -> InstructionMetadata -> Instruction

data LLVMObj = Instr LLVMInstr | Op Operand | Term Terminator

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

unwrapOp :: LLVMObj -> Either String Operand 
unwrapOp (Op op) = pure op
unwrapOp _       = fail "not an operand"

unwrapTerm :: LLVMObj -> Either String Terminator
unwrapTerm (Term t) = pure t
unwrapTerm _        = fail "not a terminator"

wasmInstrToLLVMObj :: S.Instruction Natural -> LLVMObj
-- wasmInstrToLLVMObj (S.I32Const n)      = Instr $ Op n
-- Binary Operations(IBinOPs)
wasmInstrToLLVMObj (S.IBinOp _ S.IAdd)  = Instr $ B $ BBOOI Add
wasmInstrToLLVMObj (S.IBinOp _ S.ISub)  = Instr $ B $ BBOOI Sub 
wasmInstrToLLVMObj (S.IBinOp _ S.IMul)  = Instr $ B $ BBOOI Mul
wasmInstrToLLVMObj (S.IBinOp _ S.IDivS) = Instr $ B $ BOOI SDiv
wasmInstrToLLVMObj (S.IBinOp _ S.IRemS) = Instr $ B $ OOI SRem
wasmInstrToLLVMObj (S.IBinOp _ S.IAnd ) = Instr $ B $ OOI And 
wasmInstrToLLVMObj (S.IBinOp _ S.IOr )  = Instr $ B $ OOI Or
wasmInstrToLLVMObj (S.IBinOp _ S.IXor ) = Instr $ B $ OOI Xor
wasmInstrToLLVMObj (S.IBinOp _ S.IShl ) = Instr $ B $ BBOOI Shl
-- count leading zeros, trailing zeros, popped vals (IUnOPs)//////TO FIX
--wasmInstrToLLVMObj (S.IUnOp _ S.IClz) = Instr $ -- Clz
--wasmInstrToLLVMObj (S.IUnOp _ S.ICtz) = Instr $ -- Ctz
--wasmInstrToLLVMObj (S.IUnOp _ S.IPopcnt) = Instr $ -- Popcnt
-- Relational Operations(IRelOPs) IEq	 INe	 ILtU	 ILtS	 IGtU	 IGtS	 ILeU	 ILeS	 IGeU	 IGeS
wasmInstrToLLVMObj (S.IRelOp _ S.IEq)  = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.EQ)
wasmInstrToLLVMObj (S.IRelOp _ S.INe)  = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.NE)
wasmInstrToLLVMObj (S.IRelOp _ S.ILtU) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.ULT)
wasmInstrToLLVMObj (S.IRelOp _ S.ILtS) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.SLT)
wasmInstrToLLVMObj (S.IRelOp _ S.IGtU) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.UGT)
wasmInstrToLLVMObj (S.IRelOp _ S.IGtS) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.SGT)
wasmInstrToLLVMObj (S.IRelOp _ S.ILeU) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.ULE)
wasmInstrToLLVMObj (S.IRelOp _ S.ILeS) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.SLE)
wasmInstrToLLVMObj (S.IRelOp _ S.IGeU) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.UGE)
wasmInstrToLLVMObj (S.IRelOp _ S.IGeS) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.SGE)
-- FUn Operations(FUnOPs) FAbs	 FNeg	 FCeil	 FFloor	 FTrunc	 FNearest	 FSqrt
--wasmInstrToLLVMObj (S.FUnOp _ S.FAbs) = Instr $ IOOI Abs

wasmTypeToLLVMType :: S.ValueType -> Type
wasmTypeToLLVMType S.I32 = IntegerType 32
wasmTypeToLLVMType S.I64 = IntegerType 64
wasmTypeToLLVMType S.F32 = FloatingPointType FloatFP
wasmTypeToLLVMType S.F64 = FloatingPointType DoubleFP
    
buildBasicBlock :: Name -> Named Terminator -> [LLVMObj] -> ExceptT String (State WasmModST) BasicBlock
buildBasicBlock name term llvmObjs = BasicBlock name <$> llvmInstrs <*> pure term
  where
    chunks = splitWhen (\x -> isLLVMOp x || isLLVMTerm x) llvmObjs -- llvmObj?
    llvmInstrs = concat <$> traverse munch chunks

    munch :: [LLVMObj] -> ExceptT String (State WasmModST) [Named Instruction]
    munch []    = pure []
    munch chunk = sequence [buildLLVMInstr chunk]
      where
        buildBinOp :: BinOp -> Operand -> Operand -> Instruction
        buildBinOp (BBOOI op) = iop op
        buildBinOp (FOOI  op) = fop op

        newIdentifier :: Type -> ExceptT String (State WasmModST) Name
        newIdentifier idType = do
          n     <- currentIdentifierNumber <$> get
          stack <- operandStack <$> get
          let name = Name $ toShort $ B.toStrict $ B.append "ident" $ encode $ n + 1
              identifier = LocalReference idType name
          modify (\env -> env {
             currentIdentifierNumber = n + 1,
             operandStack = identifier : stack
            })
          pure name

        -- b
        -- a
        -- sub
        -- sub a b vs sub b a
        -- doesnt the instr come after the ops in wasm?
        -- the order of a and b might need to be switched
        buildLLVMInstr :: [LLVMObj] -> ExceptT String (State WasmModST) (Named Instruction)
        buildLLVMInstr (Instr (B op):Op a:Op b:rest) = do
          let newOps = traverse unwrapOp rest
          ops        <- liftEither newOps
          modify (\env -> env { operandStack = ops ++ operandStack env})
          identifier <- newIdentifier VoidType -- FIXME: what type should we put here?
          pure $ identifier := buildBinOp op a b
        buildLLVMInstr [Instr (B op), Op a] = do
          env        <- get
          (b, rest)  <- maybe (fail "not enough operands") pure $ L.uncons $ operandStack env
          put $ env { operandStack = rest }
          identifier <- newIdentifier VoidType -- FIXME: what type should we put here?
          pure $ identifier := buildBinOp op a b
        buildLLVMInstr [Instr (B op)] = do
          env          <- get
          (a, b, rest) <- case operandStack env of
            a:b:rest -> pure (a, b, rest)
            _        -> fail "not enough operands"
          put $ env { operandStack = rest }
          identifier   <- newIdentifier VoidType -- FIXME: what type should we put here?
          pure $ identifier := buildBinOp op a b
        buildLLVMInstr _ = fail "not implemented"

wasmFuncToLLVMFunc :: Natural -> S.Function -> ExceptT String (State WasmModST) Global
wasmFuncToLLVMFunc indx func = do
  modify (\env -> env { currentFuncIndex = indx })
  funcType    <- getFunctionType <$> get
  let returnType = convertRetType $ S.results funcType
      paramList  = buildParamList $ S.params funcType
      parameters = (paramList, False)
  modify (\env -> env { currentIdentifierNumber = fromIntegral $ L.length paramList })
  namedBlks   <- liftEither $ sequence $ assignName <$> zip [0..] blks
  basicBlocks <- traverse (\(n, t, o) -> buildBasicBlock n t o) namedBlks
  pure $ functionDefaults { basicBlocks, name, returnType, parameters }
  where
    llvmObjs = wasmInstrToLLVMObj <$> Language.Wasm.Structure.body func
    blks = splitTerm <$> splitWhen isLLVMTerm llvmObjs   -- questionable criteria here
    name = Name $ toShort $ B.toStrict $ B.append "func" $ encode indx
    getFunctionType env = functionTypes env M.! (S.funcType func)

    convertRetType []  = VoidType
    convertRetType [t] = wasmTypeToLLVMType t
    convertRetType l   = StructureType True $ wasmTypeToLLVMType <$> l

    buildParamList l = do
      (ident, paramType) <- zip paramIdentifiers $ wasmTypeToLLVMType <$> l
      pure $ Parameter paramType ident []
      where
        indx = [0..] :: [Natural]
        paramIdentifiers = (Name . toShort . B.toStrict . B.append "ident" . encode) <$> indx

    splitTerm :: [LLVMObj] -> (LLVMObj, [LLVMObj])
    splitTerm = maybe (error "empty block") id  -- error because this will be a bug
              . fmap (fmap L.reverse)
              . L.uncons
              . L.reverse

    assignName :: (Int, (LLVMObj, [LLVMObj])) -> Either String (Name, Named Terminator, [LLVMObj])
    assignName (n, (t, instrs)) = do
      term <- unwrapTerm t
      pure (Name (toShort $ B.toStrict $ B.append "block" $ encode n), Name "asdf" := term, instrs)

wasmModuleToLLVMModule :: S.Module -> ExceptT String (State WasmModST) Module
wasmModuleToLLVMModule = undefined

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])

module_ :: Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd]
  }


parseWasmModule :: B.ByteString -> Either String S.Module
parseWasmModule = Wasm.parse

-- compileModule :: S.Module -> ExceptT String (State WasmModST) Module
-- compileModule mod = do
--   -- TODO: arguments from types?????
--   -- TODO: literally everything else. what even is a table
--   -- Now: set the startId in environment
--   env <- get
--   -- put $ env { startIndex = (\(StartFunction n) -> n) <$> start mod}
--   -- what needs to go in data? kdata? text?
--   -- compile each function
--   -- instrs <- traverse (uncurry compileFunction) $ zip [0..] $ functions mod
--   -- let sections = [MIPSSection "data" [], MIPSSection "kdata" [], MIPSSection "text" []]
--   -- pure $ MIPSFile "yeehaw" sections $ filter (not . null) instrs
