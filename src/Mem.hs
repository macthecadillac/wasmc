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

--
llvmMalloc :: M.Map String FunctionType
llvmMalloc = M.fromList [("llvm.malloc", FT [Type.IntegerType, Type.IntegerType] Type.IntegerType)

-- declarations of LLVM intrinsic functions
llvmIntrinsics :: [Global.Global]
llvmIntrinsics = do
  (llvmIntr, FT { arguments, returnType }) <- M.toList llvmIntrinsicsTypes
  let parameters = ([Global.Parameter t (Name.mkName "") [] | t <- arguments], False)
      name       = Name.mkName llvmIntr
  pure $ Global.functionDefaults { Global.returnType, Global.name, Global.parameters }
