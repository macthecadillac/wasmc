{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Common

-- test the binary operators
binop1 = T "i32_add" (Interface "i32_add" [I32, I32] (Just I32))
           [([I 3, I (-4)], I (-1))]
           "(module\
              \(export \"i32_add\" (func $i32_add))\
              \(func $i32_add (param $x i32) (param $y i32) (result i32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i32.add)))" 

binop2 = T "i64_add" (Interface "i64_add" [I64, I64] (Just I64))
           [([I 3, I (-4)], I (-1))]
           "(module\
              \(export \"i64_add\" (func $i64_add))\
              \(func $i64_add (param $x i64) (param $y i64) (result i64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i64.add)))" 

binop3 = T "f32_add" (Interface "f32_add" [F32, F32] (Just F32))
           [([F 3.1, F (-4.2)], F (-1.1))]
           "(module\
              \(export \"f32_add\" (func $f32_add))\
              \(func $f32_add (param $x f32) (param $y f32) (result f32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f32.add)))" 

binop4 = T "f64_add" (Interface "f64_add" [F64, F64] (Just F64))
           [([F 3.1, F (-4.2)], F (-1.1))]
           "(module\
              \(export \"f64_add\" (func $f64_add))\
              \(func $f64_add (param $x f64) (param $y f64) (result f64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f64.add)))"

binop5 = T "i32_sub" (Interface "i32_sub" [I32, I32] (Just I32))
           [([I 3, I (-4)], I 7)]
           "(module\
              \(export \"i32_sub\" (func $i32_sub))\
              \(func $i32_sub (param $x i32) (param $y i32) (result i32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i32.sub)))" 

binop6 = T "i64_sub" (Interface "i64_sub" [I64, I64] (Just I64))
           [([I 3, I (-4)], I 7)]
           "(module\
              \(export \"i64_sub\" (func $i64_sub))\
              \(func $i64_sub (param $x i64) (param $y i64) (result i64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i64.sub)))" 

binop7 = T "f32_sub" (Interface "f32_sub" [F32, F32] (Just F32))
           [([F 3.1, F (-4.2)], F 7.3)]
           "(module\
              \(export \"f32_sub\" (func $f32_sub))\
              \(func $f32_sub (param $x f32) (param $y f32) (result f32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f32.sub)))" 

binop8 = T "f64_sub" (Interface "f64_sub" [F64, F64] (Just F64))
           [([F 3.1, F (-4.2)], F 7.3)]
           "(module\
              \(export \"f64_sub\" (func $f64_sub))\
              \(func $f64_sub (param $x f64) (param $y f64) (result f64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f64.sub)))"

binop9 = T "i32_mul" (Interface "i32_mul" [I32, I32] (Just I32))
           [([I 3, I (-4)], I (-12))]
           "(module\
              \(export \"i32_mul\" (func $i32_mul))\
              \(func $i32_mul (param $x i32) (param $y i32) (result i32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i32.mul)))" 

binop10 = T "i64_mul" (Interface "i64_mul" [I64, I64] (Just I64))
           [([I 3, I (-4)], I (-12))]
           "(module\
              \(export \"i64_mul\" (func $i64_mul))\
              \(func $i64_mul (param $x i64) (param $y i64) (result i64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i64.mul)))" 

binop11 = T "f32_mul" (Interface "f32_mul" [F32, F32] (Just F32))
           [([F 3.1, F (-4.2)], F (-13.02))]
           "(module\
              \(export \"f32_mul\" (func $f32_mul))\
              \(func $f32_mul (param $x f32) (param $y f32) (result f32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f32.mul)))" 

binop12 = T "f64_mul" (Interface "f64_mul" [F64, F64] (Just F64))
           [([F 3.1, F (-4.2)], F (-13.02))]
           "(module\
              \(export \"f64_mul\" (func $f64_mul))\
              \(func $f64_mul (param $x f64) (param $y f64) (result f64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f64.mul)))"

-- test the unary operators
unop1 = T "f32_neg" (Interface "f32_neg" [F32] (Just F32))
           [([F (-3.14)], F 3.14)]
           "(module\
              \(export \"f32_neg\" (func $f32_neg))\
              \(func $f32_neg (param $x f32) (result f32)\
                 \(local.get $x)\
                 \(f32.neg)))"

unop2 = T "f64_neg" (Interface "f64_neg" [F64] (Just F64))
           [([F (-3.14)], F 3.14)]
           "(module\
              \(export \"f64_neg\" (func $f64_neg))\
              \(func $f64_neg (param $x f64) (result f64)\
                 \(local.get $x)\
                 \(f64.neg)))"

-- test the math functions
math1 = T "f32_sqrt" (Interface "f32_sqrt" [F32] (Just F32))
           [([F 1.23], F 1.109053651)]
           "(module\
              \(export \"f32_sqrt\" (func $f32_sqrt))\
              \(func $f32_sqrt (param $x f32) (result f32)\
                 \(local.get $x)\
                 \(f32.sqrt)))"

math2 = T "f64_sqrt" (Interface "f64_sqrt" [F64] (Just F64))
           [([F 1.23], F 1.109053651)]
           "(module\
              \(export \"f64_sqrt\" (func $f64_sqrt))\
              \(func $f64_sqrt (param $x f64) (result f64)\
                 \(local.get $x)\
                 \(f64.sqrt)))"

math3 = T "f32_abs" (Interface "f32_abs" [F32] (Just F32))
           [([F (-1.23)], F 1.23)]
           "(module\
              \(export \"f32_abs\" (func $f32_abs))\
              \(func $f32_abs (param $x f32) (result f32)\
                 \(local.get $x)\
                 \(f32.abs)))"

math4 = T "f64_abs" (Interface "f64_abs" [F64] (Just F64))
           [([F (-1.23)], F 1.23)]
           "(module\
              \(export \"f64_abs\" (func $f64_abs))\
              \(func $f64_abs (param $x f64) (result f64)\
                 \(local.get $x)\
                 \(f64.abs)))"

math5 = T "f32_nearest" (Interface "f32_nearest" [F32] (Just F32))
           [([F 2.5], F 2.0), ([F 2.9], F 3.0)]
           "(module\
              \(export \"f32_nearest\" (func $f32_nearest))\
              \(func $f32_nearest (param $x f32) (result f32)\
                 \(local.get $x)\
                 \(f32.nearest)))"

math6 = T "f64_nearest" (Interface "f64_nearest" [F64] (Just F64))
           [([F 2.5], F 2.0), ([F 2.9], F 3.0)]
           "(module\
              \(export \"f64_nearest\" (func $f64_nearest))\
              \(func $f64_nearest (param $x f64) (result f64)\
                 \(local.get $x)\
                 \(f64.nearest)))"

math7 = T "f32_floor" (Interface "f32_floor" [F32] (Just F32))
           [([F 3.14], F 3.0)]
           "(module\
              \(export \"f32_floor\" (func $f32_floor))\
              \(func $f32_floor (param $x f32) (result f32)\
                 \(local.get $x)\
                 \(f32.floor)))"

math8 = T "f64_floor" (Interface "f64_floor" [F64] (Just F64))
           [([F 3.14], F 3.0)]
           "(module\
              \(export \"f64_floor\" (func $f64_floor))\
              \(func $f64_floor (param $x f64) (result f64)\
                 \(local.get $x)\
                 \(f64.floor)))"

math9 = T "f32_ceil" (Interface "f32_ceil" [F32] (Just F32))
           [([F 3.14], F 4.0)]
           "(module\
              \(export \"f32_ceil\" (func $f32_ceil))\
              \(func $f32_ceil (param $x f32) (result f32)\
                 \(local.get $x)\
                 \(f32.ceil)))"

math10 = T "f64_ceil" (Interface "f64_ceil" [F64] (Just F64))
           [([F 3.14], F 4.0)]
           "(module\
              \(export \"f64_ceil\" (func $f64_ceil))\
              \(func $f64_ceil (param $x f64) (result f64)\
                 \(local.get $x)\
                 \(f64.ceil)))"

-- test control flows
control1 = T "if-else" (Interface "if_else" [I64] (Just I64))
           [([I 0], I 42), ([I 1], I 99), ([I 2], I 7)]
           "(module\
              \(export \"if_else\" (func $if_else))\
              \(func $if_else (param $a i64) (result i64)\
              \(local $x i64)\
              \(local $y i64)\
              \(i64.const 0)\
              \(local.set $x)\
              \(local.get $a)\
              \(i64.eqz)\
              \(if\
                 \(then\
                   \(i64.const 42)\
                   \(local.set $y))\
                 \(else\
                   \(local.get $a)\
                   \(i64.const 1)\
                   \(i64.eq)\
                   \(if\
                     \(then\
                       \(i64.const 99)\
                       \(local.set $y))\
                     \(else\
                       \(i64.const 7)\
                       \(local.set $y)))))\
               \(local.get $y)))"

-- load/store

-- indirect call
