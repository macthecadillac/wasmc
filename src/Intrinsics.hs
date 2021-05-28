{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Intrinsics where

import qualified Data.Map as M
import Gen

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Type as Type

true = AST.ConstantOperand $ Const.Int 1 1
false = AST.ConstantOperand $ Const.Int 1 0

llvmIntrinsicsTypes :: M.Map String FunctionType
llvmIntrinsicsTypes = M.fromList [("llvm.fabs.f32", FT [Type.float] Type.float)
                                 ,("llvm.fabs.f64", FT [Type.double] Type.double)
                                 ,("llvm.ctlz.i32", FT [Type.i32, Type.i1] Type.i32)
                                 ,("llvm.ctlz.i64", FT [Type.i64, Type.i1] Type.i64)
                                 ,("llvm.cttz.i32", FT [Type.i32, Type.i1] Type.i32)
                                 ,("llvm.cttz.i64", FT [Type.i64, Type.i1] Type.i64)
                                 ,("llvm.ctpop.i32", FT [Type.i32] Type.i32)
                                 ,("llvm.ctpop.i64", FT [Type.i64] Type.i64)
                                 ,("llvm.ceil.f32", FT [Type.float] Type.float)
                                 ,("llvm.ceil.f64", FT [Type.double] Type.double)
                                 ,("llvm.floor.f32", FT [Type.float] Type.float)
                                 ,("llvm.floor.f64", FT [Type.double] Type.double)
                                 ,("llvm.trunc.f32", FT [Type.float] Type.float)
                                 ,("llvm.trunc.f64", FT [Type.double] Type.double)
                                 ,("llvm.roundeven.f32", FT [Type.float] Type.float)
                                 ,("llvm.roundeven.f64", FT [Type.double] Type.double)
                                 ,("llvm.sqrt.f32", FT [Type.float] Type.float)
                                 ,("llvm.sqrt.f64", FT [Type.double] Type.double)
                                 ,("llvm.minnum.f32", FT [Type.float, Type.float] Type.float)
                                 ,("llvm.minnum.f64", FT [Type.double, Type.double] Type.double)
                                 ,("llvm.maxnum.f32", FT [Type.float, Type.float] Type.float)
                                 ,("llvm.maxnum.f64", FT [Type.double, Type.double] Type.double)
                                 ,("llvm.copysign.f32", FT [Type.float, Type.float] Type.float)
                                 ,("llvm.copysign.f64", FT [Type.double, Type.double] Type.double)
                                 ]

-- declarations of LLVM intrinsic functions
llvmIntrinsics :: [Global.Global]
llvmIntrinsics = do
  (llvmIntr, FT { arguments, returnType }) <- M.toList llvmIntrinsicsTypes
  let parameters = ([Global.Parameter t (Name.mkName "") [] | t <- arguments], False)
      name       = Name.mkName llvmIntr
  pure $ Global.functionDefaults { Global.returnType, Global.name, Global.parameters }
