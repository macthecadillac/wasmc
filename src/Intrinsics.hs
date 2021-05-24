{-# LANGUAGE OverloadedStrings #-}
module Intrinsics where

import qualified Data.Map as M
import qualified Gen (FunctionType(..))

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Name
import LLVM.AST.Type

llvmAbsF32 = functionDefaults { returnType = float
                              , name = "llvm.abs.f32"
                              , parameters = ([Parameter float (mkName "") []
                                              ,Parameter i1 (mkName "") []], False)
                              }

llvmAbsF64 = functionDefaults { returnType = double
                              , name = "llvm.abs.f64"
                              , parameters = ([Parameter double (mkName "") []
                                              ,Parameter i1 (mkName "") []], False)
                              }


llvmIntrinsics = [llvmAbsF32, llvmAbsF64]
llvmIntrinsicsTypes = M.fromList [("llvm.abs.f32", Gen.FT [float] i1)
                                 ,("llvm.abs.f64", Gen.FT [double] i1)]
