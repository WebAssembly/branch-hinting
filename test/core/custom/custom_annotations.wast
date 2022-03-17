(module
        (@custom "no placement" " aaa")
        (type $t (func))
        (@custom "after func" (after func) " bbb")
        (@custom "before func" (before func) " ccc")
        (@custom "after last" (after last) " ddd")
        (table 10 funcref)
        (func (type $t))
        (@custom "after import" (after import) " eee")
        (@custom "before type" (before type) " fff")
        (@custom "after data" (after data) " ggg")
        (@custom "after code" (after code) " hhh")
        (@custom "after func" (after func) " iii")
        (@custom "before func" (before func) " jjj")
        (@custom "before first" (before first) " kkk" " second string")
)

(assert_malformed
  (module quote
    "(@custom \"after first\" (after before) \"payload\")"
    "(type $t (func))"
    "(func (type $t))"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(@custom \"before last\" (after before) \"payload\")"
    "(type $t (func))"
    "(func (type $t))"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(@custom 1)"
    "(type $t (func))"
    "(func (type $t))"
  )
  "unexpected token"
)
