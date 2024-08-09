(macro def-compiler-macro (name args . body)
  (and (assoc name *compiler-macros*)
       (error "defined"))
  (print $(def-compiler-macro ,name ,args))
  (acons! name $(,args ,@body)))

(fn compiler-macroexpand (x)
  (with-global *macros* *compiler-macros*
    (macroexpand x)))
