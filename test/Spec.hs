{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Common

binop1 = T (Interface "i32_add" [I32, I32] (Just I32))
           [I 3, I (-4)]
           (I (-1))
           "(module\
              \(func $i32_add (param $x i32) (param $y i32) (result i32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i32.add)))" 

binop2 = T (Interface "i64_add" [I64, I64] (Just I64))
           [I 3, I (-4)]
           (I (-1))
           "(module\
              \(func $i64_add (param $x i64) (param $y i64) (result i64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i64.add)))" 

binop3 = T (Interface "f32_add" [F32, F32] (Just F32))
           [F 3.1, F (-4.2)]
           (F (-1.1))
           "(module\
              \(func $f32_add (param $x f32) (param $y f32) (result f32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f32.add)))" 

binop4 = T (Interface "f64_add" [F64, F64] (Just F64))
           [F 3.1, F (-4.2)]
           (F (-1.1))
           "(module\
              \(func $f64_add (param $x f64) (param $y f64) (result f64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f64.add)))"

binop5 = T (Interface "i32_sub" [I32, I32] (Just I32))
           [I 3, I (-4)]
           (I 7)
           "(module\
              \(func $i32_sub (param $x i32) (param $y i32) (result i32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i32.sub)))" 

binop6 = T (Interface "i64_sub" [I64, I64] (Just I64))
           [I 3, I (-4)]
           (I 7)
           "(module\
              \(func $i64_sub (param $x i64) (param $y i64) (result i64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i64.sub)))" 

binop7 = T (Interface "f32_sub" [F32, F32] (Just F32))
           [F 3.1, F (-4.2)]
           (F 7.3)
           "(module\
              \(func $f32_sub (param $x f32) (param $y f32) (result f32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f32.sub)))" 

binop8 = T (Interface "f64_sub" [F64, F64] (Just F64))
           [F 3.1, F (-4.2)]
           (F 7.3)
           "(module\
              \(func $f64_sub (param $x f64) (param $y f64) (result f64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f64.sub)))"

binop9 = T (Interface "i32_mul" [I32, I32] (Just I32))
           [I 3, I (-4)]
           (I (-12))
           "(module\
              \(func $i32_mul (param $x i32) (param $y i32) (result i32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i32.mul)))" 

binop10 = T (Interface "i64_mul" [I64, I64] (Just I64))
           [I 3, I (-4)]
           (I (-12))
           "(module\
              \(func $i64_mul (param $x i64) (param $y i64) (result i64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(i64.mul)))" 

binop11 = T (Interface "f32_mul" [F32, F32] (Just F32))
           [F 3.1, F (-4.2)]
           (F (-13.02))
           "(module\
              \(func $f32_mul (param $x f32) (param $y f32) (result f32)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f32.mul)))" 

binop12 = T (Interface "f64_mul" [F64, F64] (Just F64))
           [F 3.1, F (-4.2)]
           (F (-13.02))
           "(module\
              \(func $f64_mul (param $x f64) (param $y f64) (result f64)\
                 \(local.get $x)\
                 \(local.get $y)\
                 \(f64.mul)))"
