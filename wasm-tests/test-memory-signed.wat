(module
    (memory 1)

    (func (export "main") (param i32) (result i64)
       (i32.store (i32.const 0) (i32.const -1))
       (i64.load32_s (i32.const 0))
    )
)
