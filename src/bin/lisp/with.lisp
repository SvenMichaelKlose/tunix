(macro with (inits . body)
  ;"Local symbol values."
  $((,(carlist inits)
     ,@body)
    ,@(carlist (cdrlist inits))))

(print 'fn-with)(terpri)
(print (with (carlist '((a 1) (b 2)))))

(print 'with)(terpri)
(or (equal (macroexpand '(with ((a 1)
                                (b 2))
                           (print a)
                           (print b)))
           '(((a b)
               (print a)
               (print b))
             1 2))
    (error))
