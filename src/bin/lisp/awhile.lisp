(macro awhile (test . body)
  (let g (symbol)
    $(do ()
         ((not (= ,g ,test)))
       (let ! ,g
         ,@body))))