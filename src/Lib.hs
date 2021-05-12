{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib where

import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L
import Data.Tuple
import Data.Word
import Instructions
import qualified Language.Wasm as Wasm
import Language.Wasm.Structure
import qualified Language.Wasm.Lexer as L

import Numeric.Natural
import GHC.Natural

-- Right (
--   Module {
--     types = [FuncType {params = [], results = [I32]}],
--     functions = [
--       Function {
--         funcType = 0,
--         localTypes = [],
--         body = [I32Const 1,I32Const 3,IBinOp BS32 IAdd]
--       }
--     ],
--     tables = [],
--     mems = [],
--     globals = [],
--     elems = [],
--     datas = [],
--     start = Just (StartFunction 0),,
--     imports = [],
--     exports = []
--   }
-- )
--
-- (module
--   (func $main (result i32)
--     (i32.const 1)
--     (i32.const 3)
--     (i32.add)))
parseModule :: B.ByteString -> Either String Module
parseModule = Wasm.parse

data Env = Env {
  startIndex :: Maybe Natural,  -- index of the start function
  registers :: M.Map String String,  -- maps register to varname
  compiledFunctions :: M.Map Natural [MIPSInstruction],
  currentFuncIndex :: Natural,
  functionVarTypes :: [(Natural, [ValueType])]
}

getRegister :: String -> State Env String
getRegister varName =
  fst . fromMaybe (error "Undefined reference.")
      . L.find ((== varName) . snd)
      . M.assocs
      . registers
      <$> get

useNextRegister :: String -> String -> State Env String
useNextRegister prefix varName = do
  env <- get
  -- Note: This is guaranteed to succeed because we're searching through an infinite list.
  -- That being said, the register might not always be valid, but that's another problem.
  let regs = registers env
      n = fromMaybe (error "Something wrong") $ L.find ((`M.notMember` regs) . (prefix ++) . show) [0..]
      r = prefix ++ show n
  put $ env { registers = M.insert r varName regs }
  pure r

freeRegister :: String -> State Env ()
freeRegister varName = registerExists varName >>= freeIf
  where
    registerExists name = M.member name . registers <$> get :: State Env Bool
    freeIf True  = do
      env <- get
      reg <- getRegister varName
      -- maybe handle stack locations as well?
      put $ env { registers = M.delete reg $ registers env }
    freeIf _     = pure ()

varSectionSize :: [ValueType] -> Either String Word32
varSectionSize l | size <= 4294967295 = pure $ fromIntegral size
                 | otherwise          = fail "Integer overflow"
  where
    size = L.foldl' f 0 l :: Integer
    f acc I32 = 4 + acc
    f acc F32 = 4 + acc
    f acc I64 = 8 + acc
    f acc F64 = 8 + acc

-- get the offset of the variable corresponding to the given index from the
-- bottom of the frame (offset from the frame pointer value).
getStackOffset :: Natural -> ExceptT String (State Env) Word32
getStackOffset i = do
  env <- get
  let funcIndex    = currentFuncIndex env
      varTypes     = functionVarTypes env
      funcVarTypes = L.find ((==funcIndex) . fst) varTypes
  ts <- maybe (fail "Unknown function index") (pure . snd) funcVarTypes
  liftEither $ varSectionSize $ L.take (fromIntegral i) ts

type BinOp = Register -> Register -> Register -> MIPSOp
type UnOp = Register -> Register -> MIPSOp

mipsBinOp :: BinOp -> [MIPSInstruction]
mipsBinOp binop = fmap Inst instrs
  where
    instrs = OP_LW (Tmp 0) 0 SP             -- load item from the top of the stack
           : OP_LW (Tmp 1) 4 SP             -- load the next item
           : binop (Tmp 0) (Tmp 0) (Tmp 1)  -- apply binop to the two items in place in $t0
           : OP_ADDIU SP SP 4               -- deallocate one word off the stack
           : OP_SW (Tmp 0) 0 SP             -- replace the item at the top of the stack
           : []

mipsUnOp :: UnOp -> [MIPSInstruction]
mipsUnOp unop = fmap Inst instrs
  where
    instrs = OP_LW (Tmp 0) 0 SP   -- load item from the top of the stack
           : unop (Tmp 0) (Tmp 0) -- apply binop to the two items in place in $t0
           : OP_SW (Tmp 0) 0 SP   -- replace the item at the top of the stack
           : []

wasmInstrToMIPS :: Instruction Natural -> ExceptT String (State Env) [MIPSInstruction]
-- below it's incorrct. just push item to stack.
-- wasmInstrToMIPS (I32Const i) = do
--   reg <- useNextRegister "result_save" $ show i
--   pure (reg, [Inst OP_LI reg (show i) ""])

-- WASM instruction for const is just pushing to stack
-- https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-instr-numeric-mathsf-const-c
-- push to stack: sub $sp,$sp,4; sw $t2,($sp);
wasmInstrToMIPS (I32Const i) = pure $ fmap Inst instr
  where
    instr = OP_LI (Tmp 0) i     -- load literal into $t0
          : OP_SUBIU SP SP 4    -- allocate stack space
          : OP_SW (Tmp 0) 0 SP  -- save content of $t0 to stack
          : []

-- WASM type checks the 2 inputs, then performs the binop, 
-- https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop
-- popping in mips: lw $t2,($sp); addiu $sp,$sp,4
wasmInstrToMIPS (IBinOp BS32 IAdd) = pure $ mipsBinOp OP_ADD
wasmInstrToMIPS (IBinOp BS32 IMul) = pure $ mipsBinOp OP_MUL

-- SetLocal takes whatever is at the top of the stack and puts it at the memory
-- location defined by offset i
wasmInstrToMIPS (SetLocal i) = do
  offset <- getStackOffset i
  let instr = OP_LW (Tmp 0) 0 SP       -- load from top of the stack
            : OP_SW (Tmp 0) offset FP  -- store to offset location
            : OP_ADDIU SP SP 4         -- deallocate one word
            : []
  pure $ fmap Inst instr

-- GetLocal takes whatever is at the memory location defined by offset i and
-- puts it at the top of the stack
wasmInstrToMIPS (GetLocal i) = do
  offset <- getStackOffset i
  let instr = OP_LW (Tmp 0) offset FP  -- load from memory location
            : OP_SUBIU SP SP 4         -- allocate stack space
            : OP_SW (Tmp 0) 0 SP       -- store to the top of the stack
            : []
  pure $ fmap Inst instr

wasmInstrToMIPS _ = fail "Not implemented"

-- compileFunction is adapted from CMIPS (compilerElement)
-- TODO: compute the amount of stack space needed for variables using
--       `varSectionSize` defined above and increment SP to that location before
--       the first instruction from the function body is called.
--       `wasmInstrToMIPS` relies on this.
-- TODO: save-stack/restore stack -- just use the FP. It's much easier
compileFunction :: Natural -> Function -> ExceptT String (State Env) [MIPSInstruction]
compileFunction id (Function { funcType, localTypes, body })
  | null body = pure []
  | otherwise = do
    -- TODO handle localTypes later
    -- TODO handle funcType ????? 
    -- below, body ody ody ody ody
    -- TODO: add function name as comment if possible
    env    <- get
    put $ env { currentFuncIndex = id }
    instrs <- join <$> sequence (wasmInstrToMIPS <$> body)
    let startID = startIndex env
        funcStart = maybe l ifMain startID
          where
            l = "func" ++ show id ++ "start"
            ifMain n | n == id   = "main"
                     | otherwise = l
        funcEnd = "func" ++ show id ++ "end"

        body_ = instrs ++ [Empty, Label funcEnd]
        asm = Label funcStart : body_ ++ freeMemory startID ++ [Inst $ OP_JR Ret]
        fs = compiledFunctions env
    put $ env { compiledFunctions = M.insert id asm fs }
    pure asm
  where
    freeMemory Nothing  = []
    freeMemory (Just n) | id == n   = Inst <$> [OP_LI (Res 0) 10, SYSCALL]
                        | otherwise = []

compileModule :: Module -> ExceptT String (State Env) MIPSFile 
compileModule mod = do
    -- TODO: arguments from types?????
    -- TODO: literally everything else. what even is a table
    -- Now: set the startId in environment
    env <- get
    put $ env { startIndex = (\(StartFunction n) -> n) <$> start mod}
    -- what needs to go in data? kdata? text?
    -- compile each function
    instrs <- sequence (uncurry compileFunction <$> zip [0..] (functions mod))
    let sections = [MIPSSection "data" [], MIPSSection "kdata" [], MIPSSection "text" []]
    pure $ MIPSFile "yeehaw" sections $ filter (not . null) instrs
