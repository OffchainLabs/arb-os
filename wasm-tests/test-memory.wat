(module
    (memory 1)

    (func (export "main") (param i32) (result i32)
       (i32.store (i32.const 0) (i32.const 445566))
       (i32.load (i32.const 0))
    )
)
