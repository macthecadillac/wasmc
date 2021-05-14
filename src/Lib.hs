{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Structure as S
import qualified Language.Wasm.Lexer as X

-- LLVM Stuff
import LLVM.AST
import LLVM.AST.Global

import Numeric.Natural
import qualified Utils

data WasmModST = WasmModST {
  currentFuncIndex :: Natural,
  currentFunction :: [BasicBlock],
  currentBasicBlockInstrs :: [Named Instruction],
  operandStack :: [Operand]
}

data LLVMInstr = BBOOI BBOOI
               | FOOI FOOI
type BBOOI = Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> Instruction
type FOOI = FastMathFlags -> Operand -> Operand -> InstructionMetadata -> Instruction

iop :: BBOOI -> Operand -> Operand -> Instruction
iop op a b = op False False a b []

fop :: FOOI -> Operand -> Operand -> Instruction
fop op a b = op noFastMathFlags a b []

newChunk :: S.Instruction Natural -> Bool
newChunk S.Unreachable      = False
newChunk S.Nop              = False
newChunk S.Block {}         = True
newChunk S.Loop {}          = False
newChunk S.If {}            = True
newChunk S.Br {}            = True
newChunk S.BrIf {}          = True
newChunk S.BrTable {}       = True
newChunk S.Return           = True
newChunk S.Call {}          = True
newChunk S.CallIndirect {}  = True
newChunk S.Drop             = False
newChunk S.Select           = False
newChunk S.GetLocal {}      = True
newChunk S.SetLocal {}      = True
newChunk S.TeeLocal {}      = True
newChunk S.GetGlobal {}     = True
newChunk S.SetGlobal {}     = True
newChunk S.I32Load {}       = True
newChunk S.I64Load {}       = True
newChunk S.F32Load {}       = True
newChunk S.F64Load {}       = True
newChunk S.I32Load8S {}     = True
newChunk S.I32Load8U {}     = True
newChunk S.I32Load16S {}    = True
newChunk S.I32Load16U {}    = True
newChunk S.I64Load8S {}     = True
newChunk S.I64Load8U {}     = True
newChunk S.I64Load16S {}    = True
newChunk S.I64Load16U {}    = True
newChunk S.I64Load32S {}    = True
newChunk S.I64Load32U {}    = True
newChunk S.I32Store {}      = True
newChunk S.I64Store {}      = True
newChunk S.F32Store {}      = True
newChunk S.F64Store {}      = True
newChunk S.I32Store8 {}     = True
newChunk S.I32Store16 {}    = True
newChunk S.I64Store8 {}     = True
newChunk S.I64Store16 {}    = True
newChunk S.I64Store32 {}    = True
newChunk S.CurrentMemory    = False
newChunk S.GrowMemory       = False
newChunk S.I32Const {}      = False
newChunk S.I64Const {}      = False
newChunk S.F32Const {}      = False
newChunk S.F64Const {}      = False
newChunk S.IUnOp {}         = True
newChunk S.IBinOp {}        = True
newChunk S.I32Eqz           = True
newChunk S.I64Eqz           = True
newChunk S.IRelOp {}        = False
newChunk S.FUnOp {}         = False
newChunk S.FBinOp {}        = False
newChunk S.FRelOp  {}       = False
newChunk S.I32WrapI64       = False
newChunk S.ITruncFU {}      = False
newChunk S.ITruncFS {}      = False
newChunk S.I64ExtendSI32    = False
newChunk S.I64ExtendUI32    = False
newChunk S.FConvertIU {}    = False
newChunk S.FConvertIS {}    = False
newChunk S.F32DemoteF64     = False
newChunk S.F64PromoteF32    = False
newChunk S.IReinterpretF {} = False
newChunk S.FReinterpretI {} = False

wasmInstrToLLVMInstr :: S.Instruction Natural -> Maybe LLVMInstr
wasmInstrToLLVMInstr (S.IBinOp S.BS32 S.IAdd) = pure $ BBOOI Add
wasmInstrToLLVMInstr _ = undefined

wasmInstrToLLVMOperand :: S.Instruction Natural -> Maybe Operand

-- compiles wasm blocks (delineated by branches and returns--see llvm
-- Terminator) to LLVM BasicBlock
wasmInstrsToLLVMBlock :: [S.Instruction Natural] -> ExceptT String (State WasmModST) BasicBlock
-- wasmInstrToMIPS :: S.Instruction Natural -> ExceptT String (State WasmModST) [Instruction]
-- WASM instruction for const is just pushing to stack
-- https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-instr-numeric-mathsf-const-c
-- push to stack: sub $sp,$sp,4; sw $t2,($sp);

-- i32.const 3
-- i32.const 2
-- i32.sub
-- i32.const 2
-- i32.const 4
-- i32.mul
-- i32.add

wasmInstrsToLLVMBlock instrs = undefined
  where
    chunks = Utils.splitWhen newChunk instrs
    -- use utils? splitChunks l  = splitWhen newChunks l
    -- digest chunk =

-- wasmInstrToMIPS (S.I32Const i) = do
--   env <- get
--   env's stack, push Operand i

-- wasmInstrToMIPS (S.IAdd) = do
--   env <-
--   i = pop envStack
--   j = pop envStack
--   -- %k = op %i %i
--   append (k = iop Add i j) to list of compiled instructions
--   push k to envStack

-- wasmInstrToMIPS (S.I32Const i) = pure $ fmap Inst instr
--   where
--     instr = [OP_LI (Tmp 0) i      -- load literal into $t0
--            , OP_LI (Tmp 8) 4
--            , OP_SUB SP SP (Tmp 8) -- allocate stack space
--            , OP_SW (Tmp 0) 0 SP]  -- save content of $t0 to stack

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


parseModule :: B.ByteString -> Either String S.Module
parseModule = Wasm.parse

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
