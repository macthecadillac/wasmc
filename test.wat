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

;; (module
;;   (start $main)
;;   (func $main
;;     (local $x i32)
;;     ;; (local $y f32)
;;     ;; (f64.const 1.2)
;;     ;; (f64.const 1.4)
;;     ;; (local.set $y)
;;     (i32.const 1)
;;     (i32.const 3)
;;     (i32.add)
;;     (local.set $x)))

(module
  (start $main)
  (func $main (result i32)
    (i32.const 3)
    (i32.const 2)
    (i32.add)
    (i32.const 4)
    (i32.const 1)
    (i32.add)
    (i32.add)
  )
)