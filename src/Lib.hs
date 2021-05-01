{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib
    ( parseModule
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Language.Wasm as Wasm
import Language.Wasm.Structure
import qualified Language.Wasm.Lexer as L
import CodeGen.X86

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

wasmInstrToX86 :: Instruction Natural -> Either String [Code]
wasmInstrToX86 (IBinOp BS64 IAdd) = pure [pop rax, pop rbx, add rax rbx, push rax]
wasmInstrToX86 (IBinOp BS64 ISub) = pure [pop rax, pop rbx, sub rax rbx, push rax]
wasmInstrToX86 _ = fail "Not implemented"

compileFunction :: Function -> Either String Code
compileFunction (Function { funcType, localTypes, body }) = undefined
--   where

--      l = pure [Label 
