(module
  (import "test8" "foo" (func $foo (result i64)))
  (import "test8" "t" (table $t))
  (func $main (result i64)
    (call $foo)
	(return))
)
