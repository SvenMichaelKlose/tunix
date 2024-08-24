(macro with-global (n v .body)
  ;"Temporarily assign global with new value."
  (let g (symbol)
    $(block t
       (= ,g ,n)
       (= ,n ,v)
       (block nil
         ,@body)
       (= ,n ,g))))
