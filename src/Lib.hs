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
type UnOp  = Register -> Register -> MIPSOp
-- type RRWOp = Register -> Register -> Word32 -> MIPSOp

mipsBinOp ::  BinOp -> [MIPSInstruction]
mipsBinOp binop = fmap Inst instrs
  where
    instrs = [OP_LW (Tmp 0) 0 SP             -- load item from the top of the stack
            , OP_LW (Tmp 1) 4 SP             -- load the next item
            , binop (Tmp 0) (Tmp 0) (Tmp 1)  -- apply binop to the two items in place in $t0
            , OP_ADDIU SP SP 4               -- deallocate one word off the stack
            , OP_SW (Tmp 0) 0 SP]            -- replace the item at the top of the stack
  
-- mipsOpRRW :: RRWOp -> [MIPSInstruction]
-- mipsOpRRW op = fmap Inst instrs
--   where
--     instrs = [OP_LW (Tmp 0) 0 SP             -- load item from the top of the stack
--             , OP_LW (Tmp 1) 4 SP             -- load the next item
--             , op (Tmp 0) (Tmp 0) 
--     ]

mipsUnOp :: UnOp -> [MIPSInstruction]
mipsUnOp unop = fmap Inst instrs
  where
    instrs = [OP_LW (Tmp 0) 0 SP   -- load item from the top of the stack
            , unop (Tmp 0) (Tmp 0) -- apply unop to the item in place in $t0
            , OP_SW (Tmp 0) 0 SP]  -- replace the item at the top of the stack

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
    instr = [OP_LI (Tmp 0) i      -- load literal into $t0
           , OP_LI (Tmp 8) 4
           , OP_SUB SP SP (Tmp 8) -- allocate stack space
           , OP_SW (Tmp 0) 0 SP]  -- save content of $t0 to stack

-- WASM type checks the 2 inputs, then performs the binop, 
-- https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop
-- popping in mips: lw $t2,($sp); addiu $sp,$sp,4
wasmInstrToMIPS (IBinOp BS32 IAdd)  = pure $ mipsBinOp OP_ADD
wasmInstrToMIPS (IBinOp BS32 ISub)  = pure $ mipsBinOp OP_SUB
wasmInstrToMIPS (IBinOp BS32 IMul)  = pure $ mipsBinOp OP_MUL
wasmInstrToMIPS (IBinOp BS32 IDivS) = pure $ mipsBinOp OP_DIVS
wasmInstrToMIPS (IBinOp BS32 IDivU) = pure $ mipsUnOp OP_DIV
wasmInstrToMIPS (IBinOp BS32 IRemU) = pure $ mipsBinOp OP_REM
wasmInstrToMIPS (IBinOp BS32 IAnd)  = pure $ mipsBinOp OP_AND
wasmInstrToMIPS (IBinOp BS32 IOr)   = pure $ mipsBinOp OP_OR
wasmInstrToMIPS (IBinOp BS32 IXor)  = pure $ mipsBinOp OP_XOR
wasmInstrToMIPS (IBinOp BS32 IShl)  = pure $ mipsBinOp OP_SLLV
wasmInstrToMIPS (IBinOp BS32 IShrU) = pure $ mipsBinOp OP_SRLV
--- rotate shift TODO
wasmInstrToMIPS (IRelOp BS32 IEq)   = pure $ mipsBinOp OP_SEQ
wasmInstrToMIPS (IRelOp BS32 INe)   = pure $ mipsBinOp OP_SNE
wasmInstrToMIPS (IRelOp BS32 IGtS)  = pure $ mipsBinOp OP_SGT
wasmInstrToMIPS (IRelOp BS32 IGeS)  = pure $ mipsBinOp OP_SGE
wasmInstrToMIPS (IRelOp BS32 ILtS)  = pure $ mipsBinOp OP_SLT
wasmInstrToMIPS (IRelOp BS32 ILeS)  = pure $ mipsBinOp OP_SLE
wasmInstrToMIPS (IRelOp BS32 IGtU)  = pure $ mipsBinOp OP_SGTU
wasmInstrToMIPS (IRelOp BS32 IGeU)  = pure $ mipsBinOp OP_SGEU
wasmInstrToMIPS (IRelOp BS32 ILtU)  = pure $ mipsBinOp OP_SLTU
wasmInstrToMIPS (IRelOp BS32 ILeU)  = pure $ mipsBinOp OP_SLEU

-- wasmInstrToMIPS (IUnOp BS32 IClz)   = pure $ mipsUnOp OP_CL


-- SetLocal takes whatever is at the top of the stack and puts it at the memory
-- location defined by offset i
wasmInstrToMIPS (SetLocal i) = do
  offset <- getStackOffset i
  let instr = [OP_LW (Tmp 0) 0 SP       -- load from top of the stack
             , OP_SW (Tmp 0) offset FP  -- store to offset location
             , OP_ADDIU SP SP 4]        -- deallocate one word
  pure $ fmap Inst instr

-- GetLocal takes whatever is at the memory location defined by offset i and
-- puts it at the top of the stack
wasmInstrToMIPS (GetLocal i) = do
  offset <- getStackOffset i
  let instr = [OP_LW (Tmp 0) offset FP  -- load from memory location
             , OP_LI (Tmp 1) 4
             , OP_SUB SP SP (Tmp 1)     -- allocate stack space
             , OP_SW (Tmp 0) 0 SP]      -- store to the top of the stack
  pure $ fmap Inst instr

wasmInstrToMIPS (Call i) = pure [Inst $ OP_JAL $ "func" ++ show i ++ "start"]

wasmInstrToMIPS _ = fail "Not implemented"

-- compileFunction is adapted from CMIPS (compilerElement)
compileFunction :: Natural -> Function -> ExceptT String (State Env) [MIPSInstruction]
compileFunction id Function { funcType, localTypes, body }
  | null body = pure []
  | otherwise = do
    -- TODO handle localTypes later
    -- TODO: add function name as comment if possible
    env        <- get
    put $ env { currentFuncIndex = id }
    instrs     <- join <$> traverse wasmInstrToMIPS body
    varSecSize <- getStackOffset 0  -- FIXME: watch out for off-by-one
    let startID = startIndex env
        funcStart = maybe l ifMain startID
          where
            l = "func" ++ show id ++ "start"
            ifMain n | n == id   = "main"
                     | otherwise = l
        funcEnd = "func" ++ show id ++ "end"
        -- FIXME: there's probably an off-by-one error somewhere here
        allocateStackFrame = Inst <$>
                             [OP_LI (Tmp 8) 4
                            , OP_SUB SP SP (Tmp 8)        -- allocate stack space
                            , OP_SW (Tmp 9) 0 SP          -- save base of current stack frame
                            , OP_SUB SP SP (Tmp 8)        -- allocate stack frame
                            , OP_MOVE FP SP               -- bump frame pointer
                            -- add params
                            , OP_LI (Tmp 8) varSecSize
                            , OP_SUB SP SP (Tmp 8)]  -- allocate for variables
        restoreStackFrame = Inst <$> 
                            [OP_MOVE SP FP       -- deallocate all variables
                           , OP_ADDIU SP SP 4    -- point at cell with previous mem location
                           , OP_LW (Tmp 9) 0 SP  -- load base of the previous stack frame
                           , OP_SW (Tmp 9) 0 FP  -- reset frame pointer
                           , OP_ADDIU SP SP 4 ]  -- deallocate one word

        asm = Label funcStart :
              allocateStackFrame ++
              instrs ++
              restoreStackFrame ++
              [Empty, Label funcEnd] ++
              cleanup startID
        fs = compiledFunctions env
    put $ env { compiledFunctions = M.insert id asm fs }
    pure asm
  where
    cleanup Nothing  = []
    cleanup (Just n) | id == n   = Inst <$> [OP_LI (Res 0) 10, SYSCALL]
                     | otherwise = [Inst $ OP_JR RA]

compileModule :: Module -> ExceptT String (State Env) MIPSFile
compileModule mod = do
    -- TODO: arguments from types?????
    -- TODO: literally everything else. what even is a table
    -- Now: set the startId in environment
    env <- get
    put $ env { startIndex = (\(StartFunction n) -> n) <$> start mod}
    -- what needs to go in data? kdata? text?
    -- compile each function
    instrs <- traverse (uncurry compileFunction) $ zip [0..] $ functions mod
    let sections = [MIPSSection "data" [], MIPSSection "kdata" [], MIPSSection "text" []]
    pure $ MIPSFile "yeehaw" sections $ filter (not . null) instrs
