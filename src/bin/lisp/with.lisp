(macro with (inits . body)
  ;"Local symbol values."
  (? inits
     $((,(carlist inits)
        ,@body)
       ,@(carlist (cdrlist inits)))
     $(block t
        ,@body)))

(message "Testing WITH...")
(or (equal (macroexpand '(with ((a 1)
                                (b 2))
                           (print a)
                           (print b)))
           '(((a b)
               (print a)
               (print b))
             1 2))
    (error))
