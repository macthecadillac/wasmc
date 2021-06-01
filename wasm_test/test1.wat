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
    (func $f2 (param $a i32) (param $b i32) (param $c i32) (result i32)
    local.get $a
    local.get $b
    i32.add
    local.get $c
    i32.sub
    local.get $d
    i32.mul
    local.get $e
    i32.div_u)
    (func $f3 (param $a f32) (param $b f32) (param $c f32) (param $d f32) (result f32)
    local.get $a
    local.get $b
    i32.add
    local.get $c
    i32.sub
    local.get $d
    i32.mul
    local.get $e
    i32.div_u)
)
