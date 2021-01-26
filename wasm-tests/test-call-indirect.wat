(module
    (table funcref (elem $f))
    (func (export "main") (param i32) (result i32)
      (local i32)
        (call_indirect (param i32) (result i32) (i32.const 0) (local.get 0))
    )
    (func $f (param i32) (result i32)
      local.get 0
      local.get 0
      i32.add
    )
)

