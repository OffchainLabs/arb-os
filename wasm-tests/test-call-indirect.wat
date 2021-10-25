(module
    (table funcref (elem $f))
    (func (export "main") (param i64) (result i64)
        (call_indirect (param i64) (result i64) (local.get 0) (i32.const 0))
    )
    (func $f (param i64) (result i64)
      local.get 0
      local.get 0
      i64.add
    )
)

