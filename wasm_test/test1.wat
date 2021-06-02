(module
  (func $f (param $a i32) (param $b i32) (param $c i32) (param $d i32) (param $e i32) (result i32)
    local.get $a
    local.get $b
    i32.add
    local.get $c
    i32.sub
    local.get $d
    i32.mul
    local.get $e
    i32.div_u)

  (func $f1 (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.and
    local.get $a
    local.get $b
    i32.or

    i32.add

    local.get $a
    local.get $a
    i32.and
    i32.add

    local.get $b
    local.get $b
    i32.or
    i32.add

    local.get $a
    local.get $b
    i32.xor
    i32.add

    local.get $a
    local.get $a
    i32.xor
    i32.add

    local.get $a
    local.get $a
    i32.shl
    i32.add
  )

  (; testing the funops: float abs ;)
  (; abs(a) ;)
  (func $f2 (param $a f32) (result f32)
    local.get $a
    f32.abs)

  (; testing the funops: float abs ;)
  (; neg(a) ;)
  (func $f3 (param $a f32) (result f32)
    local.get $a
    f32.neg)

  (; testing the funops: float abs ;)
  (; sqrt(a) ;)
  (func $f4 (param $a f32) (result f32)
    local.get $a
    f32.sqrt)

  (; testing the funops: float abs, neg, sqrt ;)
  (; abs(a) + neg(b) + sqrt(c) ;)
  (func $f5 (param $a f32) (param $b f32) (param $c f32) (result f32)
    local.get $a
    f32.abs
    local.get $b
    f32.neg
    f32.add
    local.get $c
    f32.sqrt
    f32.add)

  (; testing the funops:  ceil floor trunc nearest;)
  (; ceil(a) ;)
  (func $f6 (param $a f32) (result f32)
    local.get $a
    f32.ceil)

  (; testing the funops:  ceil floor trunc nearest;)
  (; floor(a) ;)
  (func $f7 (param $a f32) (result f32)
    local.get $a
    f32.floor)

  (; testing the funops:  ceil floor trunc nearest;)
  (; trunc(a) ;)
  (func $f8 (param $a f32) (result f32)
    local.get $a
    f32.trunc)

  (; testing the funops:  ceil floor trunc nearest;)
  (; TODO BROKEN: currently can't link in llvm ;)
  (; nearest(a) ;)
  (func $f9 (param $a f32) (result f32)
    local.get $a
    f32.floor)

  (; testing the funops:  ceil floor trunc nearest;)
  (; // ceil(a) + floor(b) + trunc(c) + ceil(d) ;)
  (func $f10 (param $a f32) (param $b f32) (param $c f32) (param $d f32) (result f32)
    local.get $a
    f32.ceil
    local.get $b
    f32.floor
    f32.add
    local.get $c
    f32.trunc
    f32.add
    local.get $d
    f32.ceil
    f32.add)
)
