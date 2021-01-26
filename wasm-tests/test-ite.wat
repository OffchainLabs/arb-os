(module
    (func (export "main") (param i32) (result i32)
      (local i32)
        (if (result i32) (i32.eq (i32.const 123) (local.get 0))
            (then (i32.const 111))
            (else (i32.const 222))
        )
    )
)

