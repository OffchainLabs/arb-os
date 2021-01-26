(module
    (func (export "main") (param i32) (result i32)
      (local i32)
        (if (result i32) (i32.gt_u (local.get 0) (i32.const 123))
            (then (i32.const 111))
            (else (call $f (i32.const 222)))
        )
    )
    (func $f (param i32) (result i32)
      local.get 0
      local.get 0
      i32.add
    )
)

