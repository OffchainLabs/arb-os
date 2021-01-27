(module
 (global $x (mut i32) (i32.const 321))
 (global $y (mut i32) (i32.const 432))

    (func (export "main") (param i32) (result i32)
       (global.set $y (i32.add (local.get 0) (global.get $x)))
       (global.get $y)
    )
)
