(macro with-global (n v .body)
  ;"Temporarily assign global with new value."
  (let g (symbol)
    $(progn
       (= ,g ,n)
       (= ,n ,v)
       (progn
         ,@body)
       (= ,n ,g))))
