;; (module
;;   (func $printI32 (import "imports" "printI32") (param i32))
;;   (func $printBool (import "imports" "printBool") (param i32))
;;   (func $printNone (import "imports" "printNone"))
;;   (func $gt3 (param $x i32)  (result i32)
;;     (local $___RET_VAL i32)
;;     (local $___EARLY_RET i32)
;;     (i32.const 0)
;;     (local.set $___EARLY_RET)
;;     (local.get $x)
;;     (i32.const 3)
;;     (i32.gt_s)
;;     (local.set $___RET_VAL)
;;     (i32.const 1)
;;     (local.set $___EARLY_RET)
;;     (local.get $___RET_VAL))
;;   (func $lt3 (param $x i32)  (result i32)
;;     (local $___RET_VAL i32)
;;     (local $___EARLY_RET i32)
;;     (i32.const 0)
;;     (local.set $___EARLY_RET)
;;     (local.get $x)
;;     (i32.const 3)
;;     (i32.lt_s)
;;     (if
;;       (then
;;         (i32.const 1)
;;         (local.set $___RET_VAL)
;;         (i32.const 1)
;;         (local.set $___EARLY_RET))
;;       (else
;;         (i32.const 0)
;;         (local.set $___RET_VAL)
;;         (i32.const 1)
;;         (local.set $___EARLY_RET)))
;;     (local.get $___RET_VAL))
;;   (func (export "_start") 
;;     (local $___IMPL_RET i32)))

(module
  (start $main)
  (func $f (param $x i32) (param $z i32) (param $a i32) (result i32)
    (local $y i32)
    (local.get $x)
    (local.set $y)
    (local.get $y))
  (func $main (result i32)
    (i32.const 1)
    (i32.const 3)
    (i32.add)
    (call $f)))