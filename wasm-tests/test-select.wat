(module
    (func (export "main") (param i32) (result i32)
        (select (i32.const 123) (i32.const 234) (i32.eq (i32.const 123) (local.get 0)))
    )
)

