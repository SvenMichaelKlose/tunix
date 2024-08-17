(message "Testing LOAD...")
(or (eq t (load '"equality.lisp"))
    (error))
(load '"list.lisp")
