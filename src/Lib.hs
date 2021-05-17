{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Control.Monad.State.Lazy
import Control.Monad.Except
import Data.Bifunctor
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Data.Word
import Debug.Trace
import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Structure as S
import qualified Language.Wasm.Structure (Function(..))

-- LLVM Stuff
import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as Conv
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Float as Float
import qualified LLVM.AST.IntegerPredicate as Pred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Type as Type

import Numeric.Natural
import Utils (makeName, splitWhen)

data WasmModST = WasmModST { startFunctionIndex :: Maybe Natural
                           , operandStack :: [AST.Operand]
                           , currentIdentifierNumber :: Natural
                           , functionTypes :: M.Map Natural S.FuncType }
                           deriving (Show)

-- just to save some typing and make the type signatures a bit cleaner
type Codegen = ExceptT String (State WasmModST)

data LLVMInstr = B BinOp
               | U UnOp
               | Call Natural
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

instance Show LLVMTerm where
  show Lib.Ret = "Ret"

type BOI =  Bool -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BOOI =  Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type BBOOI = Bool -> Bool -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type FOOI = AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type IOOI =  Pred.IntegerPredicate -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type OOI = AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction
type MOI = Maybe AST.Operand -> AST.InstructionMetadata -> AST.Terminator

data LLVMObj = Instr LLVMInstr
             | Op AST.Operand
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

unwrapOp :: LLVMObj -> AST.Operand 
unwrapOp (Op op) = op
unwrapOp _       = error "not an operand"

unwrapTerm :: LLVMObj -> LLVMTerm
unwrapTerm (Term t) = t
unwrapTerm _        = error "not a terminator"

compileInstr :: S.Instruction Natural -> LLVMObj
compileInstr (S.I32Const n) = Op $ AST.ConstantOperand $ Constant.Int 32 $ fromIntegral n
compileInstr (S.I64Const n) = Op $ AST.ConstantOperand $ Constant.Int 64 $ fromIntegral n
compileInstr (S.F32Const n) = Op $ AST.ConstantOperand $ Constant.Float $ Float.Single n
compileInstr (S.F64Const n) = Op $ AST.ConstantOperand $ Constant.Float $ Float.Double n

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

compileInstr (S.Call i)               = Instr $ Call $ fromIntegral i

-- Terminators (return, br etc)
compileInstr S.Return = Term Lib.Ret
compileInstr instr = error $ "Not implemented in compileInstr: " ++ show instr

compileType :: S.ValueType -> Type.Type
compileType S.I32 = Type.IntegerType 32
compileType S.I64 = Type.IntegerType 64
compileType S.F32 = Type.FloatingPointType Type.FloatFP
compileType S.F64 = Type.FloatingPointType Type.DoubleFP
    
buildBasicBlock :: Name.Name -> LLVMTerm -> [LLVMObj] -> Codegen Global.BasicBlock
buildBasicBlock name term llvmObjs = Global.BasicBlock name <$> llvmInstrs <*> makeTerminator term
  where
    chunks = splitWhen (\x -> isLLVMInstr x || isLLVMTerm x) llvmObjs
    llvmInstrs = L.concat <$> traverse munch chunks

    makeTerminator :: LLVMTerm -> Codegen (AST.Named AST.Terminator)
    makeTerminator Lib.Ret = do
      retVal <- stackToRetVal . operandStack <$> get
      modify (\env -> env { operandStack = [] })
      pure $ AST.Do $ AST.Ret (Just retVal) []

    stackToRetVal []  = AST.LocalReference Type.VoidType "void"
    stackToRetVal [x] = x
    stackToRetVal l   = error "add support for multiple return vals" -- TODO: support multiple return values

    -- b
    -- a
    -- sub
    -- sub a b vs sub b a
    -- doesnt the instr come after the ops in wasm?
    -- the order of a and b might need to be switched
    munch :: [LLVMObj] -> Codegen [AST.Named AST.Instruction]
    munch = munchBackwards . L.reverse
      where
        munchBackwards [] = pure []
        munchBackwards (Instr (B op):rest) = do
          putOperandsOnStack rest
          opStack <- operandStack <$> get
          (a, b) <- case opStack of
                      a:b:rest -> pure (a, b)
                      _        -> fail "not enough operands"
          identifier <- identify $ B op
          pure [identifier AST.:= buildBinOp op a b]
        munchBackwards (Instr (Call i):rest) = do
          WasmModST { operandStack, functionTypes } <- get
          let funcType       = functionTypes M.! i
              arguments      = S.params funcType
              nArgs          = fromIntegral $ L.length $ arguments
              argumentTypes  = compileType <$> arguments
              resultType     = compileRetTypeList $ S.results funcType
              name           = makeName "func" i
              function       = Right
                             $ AST.ConstantOperand
                             $ flip Constant.GlobalReference name 
                             $ Type.FunctionType resultType argumentTypes False
          args       <- fetchOperands nArgs
          let arguments = [(operand, []) | operand <- args]
              instr     = AST.Call Nothing Conv.C [] function arguments [] []
          identifier <- newIdentifier resultType
          pure [identifier AST.:= instr]
        munchBackwards l
          | all isLLVMOp l = putOperandsOnStack l *> pure []
          | otherwise      = error $ "AST.Instruction sequence not implemented: " ++ show l -- FIXME: complete me!!

    fetchOperands :: Natural -> Codegen [AST.Operand]
    fetchOperands i = withExceptT (const "not enough operands") $ do
      env@WasmModST { operandStack } <- get
      let operands = L.take (fromIntegral i) operandStack
      guard $ L.length operands == fromIntegral i :: Codegen ()
      put $ env { operandStack = L.drop (fromIntegral i) operandStack }
      pure operands

    buildBinOp :: BinOp -> AST.Operand -> AST.Operand -> AST.Instruction
    buildBinOp (BBOOI _ op) = iop op
    buildBinOp (FOOI  _ op) = fop op
    buildBinOp o          = error $ "Not implemented for binop type: " ++ show o

    identify (B (BBOOI bs _)) = newIdentifier $ Type.IntegerType bs
    identify (B (BOOI  bs _)) = newIdentifier $ Type.IntegerType bs
    identify (B (OOI   bs _)) = newIdentifier $ Type.IntegerType bs
    identify _                = error "not implmented"

    newIdentifier :: Type.Type -> Codegen Name.Name
    newIdentifier idType = do
      WasmModST { currentIdentifierNumber, operandStack } <- get
      let name       = makeName "ident" $ currentIdentifierNumber + 1
          identifier = AST.LocalReference idType name
      modify (\env -> env {
         currentIdentifierNumber = currentIdentifierNumber + 1,
         operandStack = identifier : operandStack
        })
      pure name

    putOperandsOnStack :: [LLVMObj] -> Codegen ()
    putOperandsOnStack l | not $ all isLLVMOp l = error "Not all operands."
                         | otherwise            = modify f
                       where
                         f env = env { operandStack = newOps ++ operandStack env }
                         newOps = unwrapOp <$> l

compileRetTypeList :: [S.ValueType] -> Type.Type
compileRetTypeList []  = Type.VoidType
compileRetTypeList [t] = compileType t
compileRetTypeList l   = Type.StructureType True $ compileType <$> l

compileFunction :: Natural -> S.Function -> Codegen Global.Global
compileFunction indx func = do
  -- reset currentIdentifierNumber for each function
  modify (\env -> env { currentIdentifierNumber = 0})
  WasmModST { startFunctionIndex, operandStack, functionTypes } <- get
  let funcType   = functionTypes M.! (S.funcType func)
      returnType = compileRetTypeList $ S.results funcType
  paramList <- buildParamList $ S.params funcType
  let parameters = (paramList, False)
      blks       = splitTerm <$> splitWhen isLLVMTerm llvmObjs  -- FIXME: questionable criteria here
      namedBlks  = assignName <$> zip [0..] blks
      -- if indx == startIndex then "main" else "func{indx}". All the extra code
      -- handle the fact that startIndex may or may not have a value
      name = maybe (makeName "func" indx) id
        $ const "main" <$> (guard . (==indx) =<< startFunctionIndex)
  modify (\env -> env { currentIdentifierNumber = fromIntegral $ L.length paramList })
  basicBlocks <- traverse (\(n, t, o) -> buildBasicBlock n t o) namedBlks
  pure $ Global.functionDefaults { Global.basicBlocks
                                 , Global.name
                                 , Global.returnType
                                 , Global.parameters }
  where
    llvmObjs = compileInstr <$> Language.Wasm.Structure.body func

    assignName (n, (t, instrs)) = (makeName "block" n, t, instrs)

    buildParamList :: [S.ValueType] -> Codegen [Global.Parameter]
    buildParamList l = do  -- Codegen monad
      let nargs = fromIntegral $ L.length l
      env@WasmModST { currentIdentifierNumber } <- get
      put $ env { currentIdentifierNumber = currentIdentifierNumber + nargs }
      pure $ do  -- List monad
        (i, t) <- zip [0..] l
        pure $ Global.Parameter (compileType t) (makeName "ident" i) []

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
compileModule :: S.Module -> Either String AST.Module
compileModule wasmMod = do
  globalDefs <- buildGlobalDefs wasmMod
  pure $ AST.defaultModule
    { AST.moduleName = "basic",
      AST.moduleDefinitions = [AST.GlobalDefinition def | def <- globalDefs]
    }
  where
    startFunctionIndex = (\(S.StartFunction n) -> n) <$> S.start wasmMod
    initModST = WasmModST { startFunctionIndex
                          , operandStack = []
                          , currentIdentifierNumber = 0
                          , functionTypes = M.fromList $ zip [0..] $ S.types wasmMod
                          }
    -- extract functions from a WASM module and compile them
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
