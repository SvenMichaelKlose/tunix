(macro let (n v . body)
  ;"Local symbol value."
  $(((,n)
      ,@body)
    ,v))

(print 'let)(terpri)
(or (equal (macroexpand '(let a 1
                           (+ 2 a)))
           '(((a)
               (+ 2 a))
             1))
    (error))
