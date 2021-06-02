(module
  (table 2 funcref)
  (start $main)
  (elem (i32.const 0)
        $f11
        $f12)
  (memory 1)

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

  (func $f11 (result i32)
    i32.const 666
  )

  (type $_0 (func (result i32)))
  (func $f12 (result i32)
    i32.const 0
    call_indirect (type $_0)
    i32.const 999
    i32.add
  )

  (; calling a table function ;)
  (func $f13 (param $a i32) (result i32)
    i32.const 0
    call_indirect (type $_0)
    local.get $a
    i32.add
  )

  (; calling a nested table function ;)
  (func $f14 (result i32)
    i32.const 1
    call_indirect (type $_0)
  )

  (; if else testing ;)
  (func $f15 (param $a i64) (result i64)
    (local $x i64)
    (local $y i64)
    (i64.const 0)
    (local.set $x)
    (local.get $a)
    (i64.eqz)
    (if
      (then
        (i64.const 42)
        (local.set $y))
      (else
        (local.get $a)
        (i64.const 1)
        (i64.eq)
        (if
          (then
            (i64.const 99)
            (local.set $y))
          (else
            (i64.const 7)
            (local.set $y)))))
    (local.get $y))

  (; testing memory storage and retrieval helper ;)
  (func $f16 (result i32)
    (i32.const 0)
    (i32.const 300)
    (i32.store)
    (i32.const 47)
  ) 

  (; testing memory storage and retrieval ;)
  (func $f17 (result i32)
    (call $f16)
    (i32.const 0)
    (i32.load)
    (i32.add)
  ) 
  (;𝖼𝗅𝗓 | 𝖼𝗍𝗓 | 𝗉𝗈𝗉𝖼𝗇𝗍;)
  (func $f18 (result i32)
    (i32.const 6)
    i32.popcnt
    (i32.const 16)
    i32.ctz
    i32.add
    (i32.const 32)
    i32.clz
    i32.add
  )

  (func $main)
)
