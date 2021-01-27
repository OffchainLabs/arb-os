(module
  (func (export "main") (param i32) (result i32)
    (block
      (block
        (block
          (block
            (block
              (br_table 3 2 1 0 4 (local.get 0))
              (return (i32.const 99))
            )
            (return (i32.const 100))
          )
          (return (i32.const 101))
        )
        (return (i32.const 102))
      )
      (return (i32.const 103))
    )
    (i32.const 104)
  )
)

