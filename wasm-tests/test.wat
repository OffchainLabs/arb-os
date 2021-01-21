(module
    (func (export "main") (param i32) (result i32)
      (local i32)
        (local.set 1 (local.get 0))
        (local.set 1 (i32.add (local.get 0) (local.get 1)))
        (return (local.get 0))
    )
)

