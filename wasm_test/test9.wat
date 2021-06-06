(module
  (export "foo" (func $foo))
  (func $foo (result i64)
    (local $x i64)
    (local $y i64)
    (i64.const 0)
    (local.set $y)
    (i64.const 2)
    (local.set $x)
    (local.get $x)
    (i64.eqz)
    (if
      (then
        (i64.const 42)
        (return))
      (else
        (local.get $x)
        (i64.const 1)
        (i64.eq)
        (if
          (then
            (i64.const 99)
            (return))
          (else
            (i64.const 7)
            (return)))))
    (local.get $y)))
