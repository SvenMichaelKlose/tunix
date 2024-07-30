(macro let (n v . body)
  ;"Local symbol value."
  $(((,n)
      ,@body)
    ,v))

(message "Testing MACRO? on LET...")
(or (macro? 'let)
    (error))

(message "Testing LET...")
(or (equal (macroexpand '(let a 1
                           (+ 2 a)))
           '(((a)
               (+ 2 a))
             1))
    (error))
