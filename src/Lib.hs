{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib
    ( parseModule
    ) where

import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L
import Data.Tuple
import qualified Language.Wasm as Wasm
import Language.Wasm.Structure
import qualified Language.Wasm.Lexer as L
import Instructions

import Numeric.Natural

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
--     start = Nothing,
--     imports = [],
--     exports = [Export {name = \"_start\", desc = ExportFunc 0}]
--   }
-- )
--
-- (module
--   (func (export "_start") (result i32)
--     (i32.const 1)
--     (i32.const 3)
--     (i32.add)))
parseModule :: B.ByteString -> Either String Module
parseModule = Wasm.parse

data Env = Env {
    registers :: M.Map String String  -- maps register to varname
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
    let n = fromMaybe (error "Something wrong") $ L.find ((`M.notMember` regs) . (prefix ++) . show) [0..]
    let r = prefix ++ show n
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
        freeIf _ = pure ()

wasmInstrToMIPS :: Instruction Natural -> Either String [MIPSOp]
-- wasmInstrToMIPS (IBinOp BS64 IAdd) = pure [pop rax, pop rbx, add rax rbx, push rax]
-- wasmInstrToMIPS (IBinOp BS64 ISub) = pure [pop rax, pop rbx, sub rax rbx, push rax]
-- below it's incorrct. just push item to stack.
-- wasmInstrToMIPS (I32Const i) = do
--     reg <- useNextRegister "result_save" $ show i
--     pure (reg, [Inst OP_LI reg (show i) ""])

-- WASM instruction for const is just pushing to stack
-- https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-instr-numeric-mathsf-const-c
-- push to stack: sub $sp,$sp,4; sw $t2,($sp);
wasmInstrToMIPS (I32Const i) = pure [OP_LI (Tmp 1) 1, OP_SUBIU SP SP 4, OP_SW (Tmp 1) 0 SP]

-- WASM type checks the 2 inputs, then performs the binop, 
-- https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop
-- if they're not of type t, bs32, then trap TODO
-- popping in mips: lw $t2,($sp); addiu $sp,$sp,4
wasmInstrToMips (IBinOp BS32 IAdd) = pure instrs
    where
        instrs = pop1 ++ pop2 ++ addBoth
        pop1 = [OP_LW (Tmp 1) 0 SP, OP_ADDIU SP SP 4] 
        pop2 = [OP_LW (Tmp 2) 0 SP, OP_ADDIU SP SP 4]
        addBoth = [OP_ADD SP SP (Tmp 2), OP_SUB SP SP (Tmp 1), OP_SW (Tmp 1) 0 SP]
    

-- wasmInstrToMIPS _ = fail "Not implemented"

compileFunction :: Function -> Either String MIPSInstruction
compileFunction (Function { funcType, localTypes, body }) = undefined
--   where

--      l = pure [Label 
