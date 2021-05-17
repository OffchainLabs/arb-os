;; Test `call_indirect` operator

(module
  ;; Auxiliary definitions
  (type $proc (func))
  (type $out-i32 (func (result i32)))
  (type $out-i64 (func (result i64)))
  (type $over-i32 (func (param i32) (result i32)))
  (type $over-i64 (func (param i64) (result i64)))
  (type $i32-i64 (func (param i32 i64) (result i64)))
  (type $over-i32-duplicate (func (param i32) (result i32)))
  (type $over-i64-duplicate (func (param i64) (result i64)))

  (func $const-i32 (type $out-i32) (i32.const 0x132))
  (func $const-i64 (type $out-i64) (i64.const 0x164))

  (func $id-i32 (type $over-i32) (local.get 0))
  (func $id-i64 (type $over-i64) (local.get 0))

  (func $i32-i64 (type $i32-i64) (local.get 1))

  (func $over-i32-duplicate (type $over-i32-duplicate) (local.get 0))
  (func $over-i64-duplicate (type $over-i64-duplicate) (local.get 0))

  (table funcref
    (elem
      $const-i32 $const-i64 $const-i32 $const-i64
      $id-i32 $id-i64 $id-i32 $id-i64
      $i32-i64 $i32-i64 $i32-i64 $i32-i64
      $fac-i64 $fib-i64 $even $odd
      $runaway $mutual-runaway1 $mutual-runaway2
      $over-i32-duplicate $over-i64-duplicate
      $over-i32-duplicate $over-i64-duplicate
      $fac-i32 $fac-i32 $fac-i64
      $fib-i32 $fib-i32 $fib-i64
    )
  )

  ;; Syntax

  ;; Typing

  (func (export "type-i32") (result i32)
    (call_indirect (type $out-i32) (i32.const 0))
  )
  (func (export "type-i64") (result i64)
    (call_indirect (type $out-i64) (i32.const 1))
  )

  (func (export "type-index") (result i64)
    (call_indirect (type $over-i64) (i64.const 100) (i32.const 5))
  )

  (func (export "type-first-i32") (result i32)
    (call_indirect (type $over-i32) (i32.const 32) (i32.const 4))
  )
  (func (export "type-first-i64") (result i64)
    (call_indirect (type $over-i64) (i64.const 64) (i32.const 5))
  )

  (func (export "type-second-i64") (result i64)
    (call_indirect (type $i32-i64) (i32.const 32) (i64.const 64) (i32.const 9))
  )

  ;; Dispatch

  (func (export "dispatch") (param i32 i64) (result i64)
    (call_indirect (type $over-i64) (local.get 1) (local.get 0))
  )

  (func (export "dispatch-structural-i64") (param i32) (result i64)
    (call_indirect (type $over-i64-duplicate) (i64.const 9) (local.get 0))
  )
  (func (export "dispatch-structural-i32") (param i32) (result i32)
    (call_indirect (type $over-i32-duplicate) (i32.const 9) (local.get 0))
  )

  ;; Recursion

  (func $fac-i64 (export "fac-i64") (type $over-i64)
    (if (result i64) (i64.eqz (local.get 0))
      (then (i64.const 1))
      (else
        (i64.mul
          (local.get 0)
          (call_indirect (type $over-i64)
            (i64.sub (local.get 0) (i64.const 1))
            (i32.const 12)
          )
        )
      )
    )
  )

  (func $fib-i64 (export "fib-i64") (type $over-i64)
    (if (result i64) (i64.le_u (local.get 0) (i64.const 1))
      (then (i64.const 1))
      (else
        (i64.add
          (call_indirect (type $over-i64)
            (i64.sub (local.get 0) (i64.const 2))
            (i32.const 13)
          )
          (call_indirect (type $over-i64)
            (i64.sub (local.get 0) (i64.const 1))
            (i32.const 13)
          )
        )
      )
    )
  )

  (func $fac-i32 (export "fac-i32") (type $over-i32)
    (if (result i32) (i32.eqz (local.get 0))
      (then (i32.const 1))
      (else
        (i32.mul
          (local.get 0)
          (call_indirect (type $over-i32)
            (i32.sub (local.get 0) (i32.const 1))
            (i32.const 23)
          )
        )
      )
    )
  )


  (func $fib-i32 (export "fib-i32") (type $over-i32)
    (if (result i32) (i32.le_u (local.get 0) (i32.const 1))
      (then (i32.const 1))
      (else
        (i32.add
          (call_indirect (type $over-i32)
            (i32.sub (local.get 0) (i32.const 2))
            (i32.const 26)
          )
          (call_indirect (type $over-i32)
            (i32.sub (local.get 0) (i32.const 1))
            (i32.const 26)
          )
        )
      )
    )
  )


  (func $even (export "even") (param i32) (result i32)
    (if (result i32) (i32.eqz (local.get 0))
      (then (i32.const 44))
      (else
        (call_indirect (type $over-i32)
          (i32.sub (local.get 0) (i32.const 1))
          (i32.const 15)
        )
      )
    )
  )
  (func $odd (export "odd") (param i32) (result i32)
    (if (result i32) (i32.eqz (local.get 0))
      (then (i32.const 99))
      (else
        (call_indirect (type $over-i32)
          (i32.sub (local.get 0) (i32.const 1))
          (i32.const 14)
        )
      )
    )
  )

  ;; Stack exhaustion

  ;; Implementations are required to have every call consume some abstract
  ;; resource towards exhausting some abstract finite limit, such that
  ;; infinitely recursive test cases reliably trap in finite time. This is
  ;; because otherwise applications could come to depend on it on those
  ;; implementations and be incompatible with implementations that don't do
  ;; it (or don't do it under the same circumstances).

  (func $runaway (export "runaway") (call_indirect (type $proc) (i32.const 16)))

  (func $mutual-runaway1 (export "mutual-runaway") (call_indirect (type $proc) (i32.const 18)))
  (func $mutual-runaway2 (call_indirect (type $proc) (i32.const 17)))

  ;; As parameter of control constructs and instructions

  (memory 1)

  (func (export "as-select-first") (result i32)
    (select (call_indirect (type $out-i32) (i32.const 0)) (i32.const 2) (i32.const 3))
  )
  (func (export "as-select-mid") (result i32)
    (select (i32.const 2) (call_indirect (type $out-i32) (i32.const 0)) (i32.const 3))
  )
  (func (export "as-select-last") (result i32)
    (select (i32.const 2) (i32.const 3) (call_indirect (type $out-i32) (i32.const 0)))
  )

  (func (export "as-if-condition") (result i32)
    (if (result i32) (call_indirect (type $out-i32) (i32.const 0)) (then (i32.const 1)) (else (i32.const 2)))
  )

  (func (export "as-br_if-first") (result i64)
    (block (result i64) (br_if 0 (call_indirect (type $out-i64) (i32.const 1)) (i32.const 2)))
  )
  (func (export "as-br_if-last") (result i32)
    (block (result i32) (br_if 0 (i32.const 2) (call_indirect (type $out-i32) (i32.const 0))))
  )

  (func (export "as-br_table-first") (result i32)
    (block (result i32) (call_indirect (type $out-i32) (i32.const 2)) (i32.const 2) (br_table 0 0))
  )
  (func (export "as-br_table-last") (result i32)
    (block (result i32) (i32.const 2) (call_indirect (type $out-i32) (i32.const 0)) (br_table 0 0))
  )

  (func (export "as-store-first")
    (call_indirect (type $out-i32) (i32.const 0)) (i32.const 1) (i32.store)
  )
  (func (export "as-store-last")
    (i32.const 10) (call_indirect (type $out-i64) (i32.const 3)) (i64.store)
  )

  (func (export "as-memory.grow-value") (result i32)
    (memory.grow (call_indirect (type $out-i32) (i32.const 0)))
  )
  (func (export "as-return-value") (result i32)
    (call_indirect (type $over-i32) (i32.const 1) (i32.const 4)) (return)
  )
  (func (export "as-drop-operand")
    (call_indirect (type $over-i64) (i64.const 1) (i32.const 5)) (drop)
  )
  (func (export "as-br-value") (result i32)
    (block (result i32) (br 0 (call_indirect (type $over-i32) (i32.const 1) (i32.const 6))))
  )
  (func (export "as-local.set-value") (result i64)
    (local i64) (local.set 0 (call_indirect (type $over-i64) (i64.const 1) (i32.const 7))) (local.get 0)
  )
  (func (export "as-local.tee-value") (result i64)
    (local i64) (local.tee 0 (call_indirect (type $over-i64) (i64.const 1) (i32.const 7)))
  )
  (global $a (mut i64) (i64.const 10))
  (func (export "as-global.set-value") (result i64)
    (global.set $a (call_indirect (type $over-i64) (i64.const 1) (i32.const 7)))
    (global.get $a)
  )

  (func (export "as-load-operand") (result i32)
    (i32.load (call_indirect (type $out-i32) (i32.const 0)))
  )

  (func (export "as-binary-left") (result i32)
    (block (result i32)
      (i32.add
        (call_indirect (type $over-i32) (i32.const 1) (i32.const 4))
        (i32.const 10)
      )
    )
  )
  (func (export "as-binary-right") (result i32)
    (block (result i32)
      (i32.sub
        (i32.const 10)
        (call_indirect (type $over-i32) (i32.const 1) (i32.const 4))
      )
    )
  )

  (func (export "as-test-operand") (result i32)
    (block (result i32)
      (i32.eqz
        (call_indirect (type $over-i32) (i32.const 1) (i32.const 4))
      )
    )
  )

  (func (export "as-compare-left") (result i32)
    (block (result i32)
      (i32.le_u
        (call_indirect (type $over-i32) (i32.const 1) (i32.const 4))
        (i32.const 10)
      )
    )
  )
  (func (export "as-compare-right") (result i32)
    (block (result i32)
      (i32.ne
        (i32.const 10)
        (call_indirect (type $over-i32) (i32.const 1) (i32.const 4))
      )
    )
  )

  (func (export "as-convert-operand") (result i64)
    (block (result i64)
      (i64.extend_i32_s
        (call_indirect (type $over-i32) (i32.const 1) (i32.const 4))
      )
    )
  )

)

(assert_return (invoke "type-i32") (i32.const 0x132))
(assert_return (invoke "type-i64") (i64.const 0x164))

(assert_return (invoke "type-index") (i64.const 100))

(assert_return (invoke "type-first-i32") (i32.const 32))
(assert_return (invoke "type-first-i64") (i64.const 64))

(assert_return (invoke "type-second-i64") (i64.const 64))

(assert_return (invoke "dispatch" (i32.const 5) (i64.const 2)) (i64.const 2))
(assert_return (invoke "dispatch" (i32.const 5) (i64.const 5)) (i64.const 5))
(assert_return (invoke "dispatch" (i32.const 12) (i64.const 5)) (i64.const 120))
(assert_return (invoke "dispatch" (i32.const 13) (i64.const 5)) (i64.const 8))
(assert_return (invoke "dispatch" (i32.const 20) (i64.const 2)) (i64.const 2))
(assert_trap (invoke "dispatch" (i32.const 0) (i64.const 2)) "indirect call type mismatch")
(assert_trap (invoke "dispatch" (i32.const 15) (i64.const 2)) "indirect call type mismatch")
(assert_trap (invoke "dispatch" (i32.const 29) (i64.const 2)) "undefined element")
(assert_trap (invoke "dispatch" (i32.const -1) (i64.const 2)) "undefined element")
(assert_trap (invoke "dispatch" (i32.const 1213432423) (i64.const 2)) "undefined element")

(assert_return (invoke "dispatch-structural-i64" (i32.const 5)) (i64.const 9))
(assert_return (invoke "dispatch-structural-i64" (i32.const 12)) (i64.const 362880))
(assert_return (invoke "dispatch-structural-i64" (i32.const 13)) (i64.const 55))
(assert_return (invoke "dispatch-structural-i64" (i32.const 20)) (i64.const 9))
(assert_trap (invoke "dispatch-structural-i64" (i32.const 11)) "indirect call type mismatch")

(assert_return (invoke "dispatch-structural-i32" (i32.const 4)) (i32.const 9))
(assert_return (invoke "dispatch-structural-i32" (i32.const 23)) (i32.const 362880))
(assert_return (invoke "dispatch-structural-i32" (i32.const 26)) (i32.const 55))
(assert_return (invoke "dispatch-structural-i32" (i32.const 19)) (i32.const 9))
(assert_trap (invoke "dispatch-structural-i32" (i32.const 9)) "indirect call type mismatch")

(assert_return (invoke "fac-i64" (i64.const 0)) (i64.const 1))
(assert_return (invoke "fac-i64" (i64.const 1)) (i64.const 1))
(assert_return (invoke "fac-i64" (i64.const 5)) (i64.const 120))
(assert_return (invoke "fac-i64" (i64.const 25)) (i64.const 7034535277573963776))

(assert_return (invoke "fac-i32" (i32.const 0)) (i32.const 1))
(assert_return (invoke "fac-i32" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fac-i32" (i32.const 5)) (i32.const 120))
(assert_return (invoke "fac-i32" (i32.const 10)) (i32.const 3628800))

(assert_return (invoke "fib-i64" (i64.const 0)) (i64.const 1))
(assert_return (invoke "fib-i64" (i64.const 1)) (i64.const 1))
(assert_return (invoke "fib-i64" (i64.const 2)) (i64.const 2))
(assert_return (invoke "fib-i64" (i64.const 5)) (i64.const 8))
(assert_return (invoke "fib-i64" (i64.const 20)) (i64.const 10946))

(assert_return (invoke "fib-i32" (i32.const 0)) (i32.const 1))
(assert_return (invoke "fib-i32" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fib-i32" (i32.const 2)) (i32.const 2))
(assert_return (invoke "fib-i32" (i32.const 5)) (i32.const 8))
(assert_return (invoke "fib-i32" (i32.const 20)) (i32.const 10946))

(assert_return (invoke "even" (i32.const 0)) (i32.const 44))
(assert_return (invoke "even" (i32.const 1)) (i32.const 99))
(assert_return (invoke "even" (i32.const 100)) (i32.const 44))
(assert_return (invoke "even" (i32.const 77)) (i32.const 99))
(assert_return (invoke "odd" (i32.const 0)) (i32.const 99))
(assert_return (invoke "odd" (i32.const 1)) (i32.const 44))
(assert_return (invoke "odd" (i32.const 200)) (i32.const 99))
(assert_return (invoke "odd" (i32.const 77)) (i32.const 44))

(assert_exhaustion (invoke "runaway") "call stack exhausted")
(assert_exhaustion (invoke "mutual-runaway") "call stack exhausted")

(assert_return (invoke "as-select-first") (i32.const 0x132))
(assert_return (invoke "as-select-mid") (i32.const 2))
(assert_return (invoke "as-select-last") (i32.const 2))

(assert_return (invoke "as-if-condition") (i32.const 1))

(assert_return (invoke "as-br_if-first") (i64.const 0x164))
(assert_return (invoke "as-br_if-last") (i32.const 2))

(assert_return (invoke "as-br_table-last") (i32.const 2))

(assert_return (invoke "as-store-first"))
(assert_return (invoke "as-store-last"))

(assert_return (invoke "as-memory.grow-value") (i32.const 1))
(assert_return (invoke "as-return-value") (i32.const 1))
(assert_return (invoke "as-drop-operand"))
(assert_return (invoke "as-load-operand") (i32.const 1))

(assert_return (invoke "as-binary-left") (i32.const 11))
(assert_return (invoke "as-binary-right") (i32.const 9))
(assert_return (invoke "as-test-operand") (i32.const 0))
(assert_return (invoke "as-compare-left") (i32.const 1))
(assert_return (invoke "as-compare-right") (i32.const 1))
(assert_return (invoke "as-convert-operand") (i64.const 1))

;; Invalid syntax

(assert_malformed
  (module quote
    "(type $sig (func (param i32) (result i32)))"
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (type $sig) (result i32) (param i32)"
    "    (i32.const 0) (i32.const 0)"
    "  )"
    ")"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(type $sig (func (param i32) (result i32)))"
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (param i32) (type $sig) (result i32)"
    "    (i32.const 0) (i32.const 0)"
    "  )"
    ")"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(type $sig (func (param i32) (result i32)))"
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (param i32) (result i32) (type $sig)"
    "    (i32.const 0) (i32.const 0)"
    "  )"
    ")"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(type $sig (func (param i32) (result i32)))"
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (result i32) (type $sig) (param i32)"
    "    (i32.const 0) (i32.const 0)"
    "  )"
    ")"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(type $sig (func (param i32) (result i32)))"
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (result i32) (param i32) (type $sig)"
    "    (i32.const 0) (i32.const 0)"
    "  )"
    ")"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (result i32) (param i32) (i32.const 0) (i32.const 0))"
    ")"
  )
  "unexpected token"
)

(assert_malformed
  (module quote
    "(table 0 funcref)"
    "(func (call_indirect (param $x i32) (i32.const 0) (i32.const 0)))"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(type $sig (func))"
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (type $sig) (result i32) (i32.const 0))"
    ")"
  )
  "inline function type"
)
(assert_malformed
  (module quote
    "(type $sig (func (param i32) (result i32)))"
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (type $sig) (result i32) (i32.const 0))"
    ")"
  )
  "inline function type"
)
(assert_malformed
  (module quote
    "(type $sig (func (param i32) (result i32)))"
    "(table 0 funcref)"
    "(func"
    "  (call_indirect (type $sig) (param i32) (i32.const 0) (i32.const 0))"
    ")"
  )
  "inline function type"
)
(assert_malformed
  (module quote
    "(type $sig (func (param i32 i32) (result i32)))"
    "(table 0 funcref)"
    "(func (result i32)"
    "  (call_indirect (type $sig) (param i32) (result i32)"
    "    (i32.const 0) (i32.const 0)"
    "  )"
    ")"
  )
  "inline function type"
)

;; Invalid typing

(assert_invalid
  (module
    (type (func))
    (func $no-table (call_indirect (type 0) (i32.const 0)))
  )
  "unknown table"
)

(assert_invalid
  (module
    (type (func))
    (table 0 funcref)
    (func $type-void-vs-num (i32.eqz (call_indirect (type 0) (i32.const 0))))
  )
  "type mismatch"
)
(assert_invalid
  (module
    (type (func (result i64)))
    (table 0 funcref)
    (func $type-num-vs-num (i32.eqz (call_indirect (type 0) (i32.const 0))))
  )
  "type mismatch"
)

(assert_invalid
  (module
    (type (func (param i32)))
    (table 0 funcref)
    (func $arity-0-vs-1 (call_indirect (type 0) (i32.const 0)))
  )
  "type mismatch"
)
(assert_invalid
  (module
    (type (func (param f64 i32)))
    (table 0 funcref)
    (func $arity-0-vs-2 (call_indirect (type 0) (i32.const 0)))
  )
  "type mismatch"
)
(assert_invalid
  (module
    (type (func))
    (table 0 funcref)
    (func $arity-1-vs-0 (call_indirect (type 0) (i32.const 1) (i32.const 0)))
  )
  "type mismatch"
)
(assert_invalid
  (module
    (type (func))
    (table 0 funcref)
    (func $arity-2-vs-0
      (call_indirect (type 0) (f64.const 2) (i32.const 1) (i32.const 0))
    )
  )
  "type mismatch"
)

(assert_invalid
  (module
    (type (func (param i32)))
    (table 0 funcref)
    (func $type-func-void-vs-i32 (call_indirect (type 0) (i32.const 1) (nop)))
  )
  "type mismatch"
)
(assert_invalid
  (module
    (type (func (param i32)))
    (table 0 funcref)
    (func $type-func-num-vs-i32 (call_indirect (type 0) (i32.const 0) (i64.const 1)))
  )
  "type mismatch"
)

(assert_invalid
  (module
    (type (func (param i32 i32)))
    (table 0 funcref)
    (func $type-first-void-vs-num
      (call_indirect (type 0) (nop) (i32.const 1) (i32.const 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (type (func (param i32 i32)))
    (table 0 funcref)
    (func $type-second-void-vs-num
      (call_indirect (type 0) (i32.const 1) (nop) (i32.const 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (type (func (param i32 f64)))
    (table 0 funcref)
    (func $type-first-num-vs-num
      (call_indirect (type 0) (f64.const 1) (i32.const 1) (i32.const 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (type (func (param f64 i32)))
    (table 0 funcref)
    (func $type-second-num-vs-num
      (call_indirect (type 0) (i32.const 1) (f64.const 1) (i32.const 0))
    )
  )
  "type mismatch"
)

(assert_invalid
  (module
    (func $f (param i32))
    (type $sig (func (param i32)))
    (table funcref (elem $f))
    (func $type-first-empty-in-block
      (block
        (call_indirect (type $sig) (i32.const 0))
      )
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $f (param i32 i32))
    (type $sig (func (param i32 i32)))
    (table funcref (elem $f))
    (func $type-second-empty-in-block
      (block
        (call_indirect (type $sig) (i32.const 0) (i32.const 0))
      )
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $f (param i32))
    (type $sig (func (param i32)))
    (table funcref (elem $f))
    (func $type-first-empty-in-loop
      (loop
        (call_indirect (type $sig) (i32.const 0))
      )
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $f (param i32 i32))
    (type $sig (func (param i32 i32)))
    (table funcref (elem $f))
    (func $type-second-empty-in-loop
      (loop
        (call_indirect (type $sig) (i32.const 0) (i32.const 0))
      )
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $f (param i32))
    (type $sig (func (param i32)))
    (table funcref (elem $f))
    (func $type-first-empty-in-then
      (i32.const 0) (i32.const 0)
      (if
        (then
          (call_indirect (type $sig) (i32.const 0))
        )
      )
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $f (param i32 i32))
    (type $sig (func (param i32 i32)))
    (table funcref (elem $f))
    (func $type-second-empty-in-then
      (i32.const 0) (i32.const 0)
      (if
        (then
          (call_indirect (type $sig) (i32.const 0) (i32.const 0))
        )
      )
    )
  )
  "type mismatch"
)


;; Unbound type

(assert_invalid
  (module
    (table 0 funcref)
    (func $unbound-type (call_indirect (type 1) (i32.const 0)))
  )
  "unknown type"
)
(assert_invalid
  (module
    (table 0 funcref)
    (func $large-type (call_indirect (type 1012321300) (i32.const 0)))
  )
  "unknown type"
)


;; Unbound function in table

(assert_invalid
  (module (table funcref (elem 0 0)))
  "unknown function"
)
