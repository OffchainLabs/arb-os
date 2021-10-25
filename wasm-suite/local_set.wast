;; Test `local.set` operator

(module
  ;; Typing

  (func (export "type-local-i32") (local i32) (local.set 0 (i32.const 0)))
  (func (export "type-local-i64") (local i64) (local.set 0 (i64.const 0)))

  (func (export "type-param-i32") (param i32) (local.set 0 (i32.const 10)))
  (func (export "type-param-i64") (param i64) (local.set 0 (i64.const 11)))


  ;; As parameter of control constructs and instructions

  (func (export "as-block-value") (param i32)
    (block (local.set 0 (i32.const 1)))
  )
  (func (export "as-loop-value") (param i32)
    (loop (local.set 0 (i32.const 3)))
  )

  (func (export "as-br-value") (param i32)
    (block (br 0 (local.set 0 (i32.const 9))))
  )
  (func (export "as-br_if-value") (param i32)
    (block
      (br_if 0 (local.set 0 (i32.const 8)) (i32.const 1))
    )
  )
  (func (export "as-br_if-value-cond") (param i32)
    (block
      (br_if 0 (i32.const 6) (local.set 0 (i32.const 9)))
    )
  )
  (func (export "as-br_table-value") (param i32)
    (block
      (br_table 0 (local.set 0 (i32.const 10)) (i32.const 1))
    )
  )

  (func (export "as-return-value") (param i32)
    (return (local.set 0 (i32.const 7)))
  )

  (func (export "as-if-then") (param i32)
    (if (local.get 0) (then (local.set 0 (i32.const 3))))
  )
  (func (export "as-if-else") (param i32)
    (if (local.get 0) (then) (else (local.set 0 (i32.const 1))))
  )
)

(assert_return (invoke "type-local-i32"))
(assert_return (invoke "type-local-i64"))

(assert_return (invoke "type-param-i32" (i32.const 2)))
(assert_return (invoke "type-param-i64" (i64.const 3)))

(assert_return (invoke "as-block-value" (i32.const 0)))
(assert_return (invoke "as-loop-value" (i32.const 0)))

(assert_return (invoke "as-br-value" (i32.const 0)))
(assert_return (invoke "as-br_if-value" (i32.const 0)))
(assert_return (invoke "as-br_if-value-cond" (i32.const 0)))
(assert_return (invoke "as-br_table-value" (i32.const 0)))

(assert_return (invoke "as-return-value" (i32.const 0)))

(assert_return (invoke "as-if-then" (i32.const 1)))
(assert_return (invoke "as-if-else" (i32.const 0)))



;; Invalid typing of access to locals


(assert_invalid
  (module (func $type-local-arg-void-vs-num (local i32) (local.set 0 (nop))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-local-arg-num-vs-num (local i32) (local.set 0 (f32.const 0))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-local-arg-num-vs-num (local f32) (local.set 0 (f64.const 0))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-local-arg-num-vs-num (local f64 i64) (local.set 1 (f64.const 0))))
  "type mismatch"
)


;; Invalid typing of access to parameters


(assert_invalid
  (module (func $type-param-arg-void-vs-num (param i32) (local.set 0 (nop))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-param-arg-num-vs-num (param i32) (local.set 0 (f32.const 0))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-param-arg-num-vs-num (param f32) (local.set 0 (f64.const 0))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-param-arg-num-vs-num (param f64 i64) (local.set 1 (f64.const 0))))
  "type mismatch"
)

(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num (param i32)
      (local.set 0)
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-block (param i32)
      (i32.const 0)
      (block (local.set 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-loop (param i32)
      (i32.const 0)
      (loop (local.set 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-then (param i32)
      (i32.const 0)
      (if (i32.const 1) (then (local.set 0)))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-else (param i32)
      (i32.const 0)
      (if (result i32) (i32.const 0) (then (i32.const 0)) (else (local.set 0)))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-br (param i32)
      (i32.const 0)
      (block (br 0 (local.set 0)))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-br_if (param i32)
      (i32.const 0)
      (block (br_if 0 (local.set 0)))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-br_table (param i32)
      (i32.const 0)
      (block (br_table 0 (local.set 0)))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-return (param i32)
      (return (local.set 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-select (param i32)
      (select (local.set 0) (i32.const 1) (i32.const 2))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $type-param-arg-empty-vs-num-in-call (param i32)
      (call 1 (local.set 0))
    )
    (func (param i32) (result i32) (local.get 0))
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $f (param i32) (result i32) (local.get 0))
    (type $sig (func (param i32) (result i32)))
    (table funcref (elem $f))
    (func $type-param-arg-empty-vs-num-in-call_indirect (param i32)
      (block (result i32)
        (call_indirect (type $sig)
          (local.set 0) (i32.const 0)
        )
      )
    )
  )
  "type mismatch"
)


;; Invalid typing of access to mixed args

(assert_invalid
  (module (func $type-mixed-arg-num-vs-num (param f32) (local i32) (local.set 1 (f32.const 0))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-mixed-arg-num-vs-num (param i64 i32) (local f32) (local.set 1 (f32.const 0))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-mixed-arg-num-vs-num (param i64) (local f64 i64) (local.set 1 (i64.const 0))))
  "type mismatch"
)


;; local.set should have no retval

(assert_invalid
  (module (func $type-empty-vs-i32 (param i32) (result i32) (local.set 0 (i32.const 1))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-empty-vs-i64 (param i64) (result i64) (local.set 0 (i64.const 1))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-empty-vs-f32 (param f32) (result f32) (local.set 0 (f32.const 1))))
  "type mismatch"
)
(assert_invalid
  (module (func $type-empty-vs-f64 (param f64) (result f64) (local.set 0 (f64.const 1))))
  "type mismatch"
)


;; Invalid local index

(assert_invalid
  (module (func $unbound-local (local i32 i64) (local.set 3 (i32.const 0))))
  "unknown local"
)
(assert_invalid
  (module (func $large-local (local i32 i64) (local.set 14324343 (i32.const 0))))
  "unknown local"
)

(assert_invalid
  (module (func $unbound-param (param i32 i64) (local.set 2 (i32.const 0))))
  "unknown local"
)
(assert_invalid
  (module (func $large-param (param i32 i64) (local.set 714324343 (i32.const 0))))
  "unknown local"
)

(assert_invalid
  (module (func $unbound-mixed (param i32) (local i32 i64) (local.set 3 (i32.const 0))))
  "unknown local"
)
(assert_invalid
  (module (func $large-mixed (param i64) (local i32 i64) (local.set 214324343 (i32.const 0))))
  "unknown local"
)

