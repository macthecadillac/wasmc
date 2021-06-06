(module
  (table 6 funcref)
  (memory 10)
  (start $main)
  (elem (i32.const 0)
        $f
        $f
        $g)
  (elem (i32.const 3) $f)
  (elem (i32.const 4) $f)
  (elem (i32.const 5) $g)
  (type $_0 (func (param i32) (param i32) (result i32)))
  (type $_1 (func (param i32)))
  (type $_2 (func ))
  (func $f (param $x i32))
  (func $g)
  (func $main
    (i32.const 10)
    (i32.const 1)
    (call_indirect (type $_1)))
  )
