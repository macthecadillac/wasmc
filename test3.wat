(module
  (export "foo" (func $foo))
  (func $foo (result i32)
    (local $x i32)
    (local $y i32)
    (i32.const 2)
    (local.set $x)
    (block
      (block
        (block
          (; x == 0 ;)
          (local.get $x)
          (i32.eqz)
          (br_if 0)
          (; x == 1 ;)
          (local.get $x)
          (i32.const 1)
          (i32.eq)
          (br_if 1)
          (; the `else` case ;)
          (i32.const 7)
          (local.set $y)
          (br 2))
        (i32.const 42)
        (local.set $y)
        (br 1))
      (i32.const 99)
      (local.set $y)
          )
    (local.get $y)))
