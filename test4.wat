(module
  (func $a (param $self i32)
    (; Variable declarations ;)
    (; Body ;))
  (func $b (param $self i32) (param $x i32)
    (i32.const 0)
    (block $a
      (i32.const 0)
      (local.set $x)
      (local.get $self)
      (local.get $x)
      (i32.add)
      (local.set $x))
    (local.set $self)
    (; Variable declarations ;)
    (; Body ;))
  (func (export "_start") (result i32)
    (local $$___IMPL_RET i32)
	(local $x i32)
	(local $y i32)
	(i32.const 3)
	(i32.const 0)
	(i32.sub)
	(i32.const 2)
	(i32.add)
	(local.set $x)
	(i32.const -2)
	(local.get $x)
	(i32.mul)
	(i32.const 4)
	(i32.const 10)
	(i32.add)
	(i32.sub)
	(local.set $y)
	(i32.const 0)
	(i32.const 1)
	(call $b)
	(i32.const 1)
    (; Variable declarations ;)
    (; Set up the VTBL ;)
    (; Body ;)))
