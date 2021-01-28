(module
    (memory 0)
    (func (export "main") (param i32) (result i32)
       (memory.grow (local.get 0))
    )
)
