(module
  (type (;0;) (func (param i32)))
  (memory (;0;) 1 1)
  (func $test (type 0)
    (local i32)
    local.get 1
    local.get 0
    i32.eq
    (@metadata.code.branch_hint "\00") if
      return
    end
    return
  )
  (export "test" (func $test))
)
