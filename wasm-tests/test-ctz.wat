(module
    (func (export "main") (param i32) (result i32)
       (i32.ctz (local.get 0))
    )
)
