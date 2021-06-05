(module
  (export "foo" (func $foo))
  (func $foo (result i64)
    (local $x i64)
    (local $y i64)
    (i64.const 2)
    (local.set $x)
    (local.get $x)
    (i64.eqz)
    (if
      (then)
      (else
        (local.get $x)
        (i64.const 1)
        (i64.eq)
        (if
          (then
            (i64.const 99)
            (local.set $y)))))
    (local.get $y)))
