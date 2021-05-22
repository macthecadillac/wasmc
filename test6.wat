(module
  (export "foo" (func $foo))
  (func $foo (result i32)
    (local $x i32)
    (local $y i32)
    (i32.const 2)
    (local.set $x)
    (local.get $x)
    (i32.eqz)
    (if
      (then
        (i32.const 42)
        (local.set $y))
      (else
        (local.get $x)
        (i32.const 1)
        (i32.eq)
        (if
          (then
            (i32.const 99)
            (local.set $y))
          (else
            (i32.const 7)
            (local.set $y)))))
    (local.get $y)))
