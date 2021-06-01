{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Mem where

import qualified Data.Map as M
import Gen

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Type as Type


--true = AST.ConstantOperand $ Const.Int 1 1
--false = AST.ConstantOperand $ Const.Int 1 0

-- https://releases.llvm.org/1.1/docs/LangRef.html#i_malloc
-- void *malloc(size_t size)
--   <result> = malloc <type>, uint <NumElements>     ; yields {type*}:result
--   free <type> <value>                              ; yields {void}
-- Type.ptr - An abbreviation for PointerType t (AddrSpace 0)
-- Type.NamedTypeReference "PointerType"


llvmMalloc :: M.Map String FunctionType
llvmMalloc = M.fromList [("malloc", FT [Type.i32, Type.i32] (Type.ptr Type.void)),
                         ("free", FT [Type.ptr Type.void] Type.void)]

-- declarations of LLVM intrinsic functions
llvmMallocDec :: [Global.Global]
llvmMallocDec = do
  (llvmIntr, FT { arguments, returnType }) <- M.toList llvmMalloc
  let parameters = ([Global.Parameter t (Name.mkName "") [] | t <- arguments], False)
      name       = Name.mkName llvmIntr
  pure $ Global.functionDefaults { Global.returnType, Global.name, Global.parameters }
