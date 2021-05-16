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
  startFunctionIndex :: Maybe Natural,
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

unwrapOp :: LLVMObj -> Operand 
unwrapOp (Op op) = op
unwrapOp _       = error "not an operand"

unwrapTerm :: LLVMObj -> Terminator
unwrapTerm (Term t) = t
unwrapTerm _        = error "not a terminator"

compileInstr :: S.Instruction Natural -> LLVMObj
-- compileInstr (S.I32Const n)      = Instr $ Op n
-- Binary Operations(IBinOPs)
compileInstr (S.IBinOp _ S.IAdd)  = Instr $ B $ BBOOI Add
compileInstr (S.IBinOp _ S.ISub)  = Instr $ B $ BBOOI Sub 
compileInstr (S.IBinOp _ S.IMul)  = Instr $ B $ BBOOI Mul
compileInstr (S.IBinOp _ S.IDivS) = Instr $ B $ BOOI SDiv
compileInstr (S.IBinOp _ S.IRemS) = Instr $ B $ OOI SRem
compileInstr (S.IBinOp _ S.IAnd ) = Instr $ B $ OOI And 
compileInstr (S.IBinOp _ S.IOr )  = Instr $ B $ OOI Or
compileInstr (S.IBinOp _ S.IXor ) = Instr $ B $ OOI Xor
compileInstr (S.IBinOp _ S.IShl ) = Instr $ B $ BBOOI Shl
-- count leading zeros, trailing zeros, popped vals (IUnOPs)//////TO FIX
--compileInstr (S.IUnOp _ S.IClz) = Instr $ -- Clz
--compileInstr (S.IUnOp _ S.ICtz) = Instr $ -- Ctz
--compileInstr (S.IUnOp _ S.IPopcnt) = Instr $ -- Popcnt
-- Relational Operations(IRelOPs) IEq	 INe	 ILtU	 ILtS	 IGtU	 IGtS	 ILeU	 ILeS	 IGeU	 IGeS
compileInstr (S.IRelOp _ S.IEq)  = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.EQ)
compileInstr (S.IRelOp _ S.INe)  = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.NE)
compileInstr (S.IRelOp _ S.ILtU) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.ULT)
compileInstr (S.IRelOp _ S.ILtS) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.SLT)
compileInstr (S.IRelOp _ S.IGtU) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.UGT)
compileInstr (S.IRelOp _ S.IGtS) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.SGT)
compileInstr (S.IRelOp _ S.ILeU) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.ULE)
compileInstr (S.IRelOp _ S.ILeS) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.SLE)
compileInstr (S.IRelOp _ S.IGeU) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.UGE)
compileInstr (S.IRelOp _ S.IGeS) = Instr $ B $ OOI (ICmp LLVM.AST.IntegerPredicate.SGE)
-- FUn Operations(FUnOPs) FAbs	 FNeg	 FCeil	 FFloor	 FTrunc	 FNearest	 FSqrt
--compileInstr (S.FUnOp _ S.FAbs) = Instr $ IOOI Abs

compileType :: S.ValueType -> Type
compileType S.I32 = IntegerType 32
compileType S.I64 = IntegerType 64
compileType S.F32 = FloatingPointType FloatFP
compileType S.F64 = FloatingPointType DoubleFP
    
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
          let newOps = unwrapOp <$> rest
          modify (\env -> env { operandStack = newOps ++ operandStack env})
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
        buildLLVMInstr _ = error "not implemented" -- FIXME: complete me!!

compileFunction :: Natural -> S.Function -> ExceptT String (State WasmModST) Global
compileFunction indx func = do
  startIndex <- startFunctionIndex <$> get
  funcType   <- getFunctionType <$> get
  let returnType = compileRetType $ S.results funcType
      paramList  = buildParamList $ S.params funcType
      parameters = (paramList, False)
      namedBlks  = assignName <$> zip [0..] blks
      -- if indx == startIndex then "main" else "func{indx}". All the extra code
      -- handle the fact that startIndex may or may not have a value
      name = maybe (newName "func" indx) id $ const "main" <$> (guard . (==indx) =<< startIndex)
  modify (\env -> env { currentIdentifierNumber = fromIntegral $ L.length paramList })
  basicBlocks <- traverse (\(n, t, o) -> buildBasicBlock n t o) namedBlks
  pure $ functionDefaults { basicBlocks, name, returnType, parameters }
  where
    llvmObjs = compileInstr <$> Language.Wasm.Structure.body func
    blks = splitTerm <$> splitWhen isLLVMTerm llvmObjs   -- FIXME: questionable criteria here
    getFunctionType env = functionTypes env M.! (S.funcType func)

    assignName (n, (t, instrs)) = (newName "block" n, Name "asdf" := unwrapTerm t, instrs)

    compileRetType []  = VoidType
    compileRetType [t] = compileType t
    compileRetType l   = StructureType True $ compileType <$> l

    buildParamList l = do
      (ident, paramType) <- zip paramIdentifiers $ compileType <$> l
      pure $ Parameter paramType ident []
      where
        indx = [0..] :: [Natural]
        paramIdentifiers = newName "ident" <$> indx

    splitTerm :: [LLVMObj] -> (LLVMObj, [LLVMObj])
    splitTerm = maybe (error "empty block") id  -- error because this will be a bug
              . fmap (fmap L.reverse)
              . L.uncons
              . L.reverse

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
