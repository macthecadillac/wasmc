(module
  (export "foo" (func $foo))
  (func $foo
	(loop
	  (br 0))
	))
