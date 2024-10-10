(macro with-global (n v . body)
  ;"Temporarily assign global with new value."
  (let (g (symbol))
    $(let (,g ,n)
       (= ,n ,v)
       (prog1
         (progn
           ,@body)
         (= ,n ,g)))))
