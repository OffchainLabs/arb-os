(module
    (func (export "main") (param i32) (result i32) (local i32)
      (call $f (i32.const 222) (local.get 0))
    )
    (func $f (param i32 i32) (result i32)
      local.get 1
    )
)

