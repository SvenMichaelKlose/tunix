(macro with-global (n v . body)
  ;"Temporarily assign global with new value."
  (with (g (symbol))
    $(with (,g ,n)
       (= ,n ,v)
       (prog1
         (progn
           ,@body)
         (= ,n ,g)))))
