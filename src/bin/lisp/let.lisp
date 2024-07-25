(macro let (n v . body)
  ;"Local symbol value."
  $(((,n)
      ,@body)
    ,v))

(out "Testing LET...")(terpri)
(or (equal (macroexpand '(let a 1
                           (+ 2 a)))
           '(((a)
               (+ 2 a))
             1))
    (error))
