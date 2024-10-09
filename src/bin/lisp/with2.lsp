(macro with* (inits . body)
  ;"Local symbol values."
  (? inits
     $((,(@ car inits)
        ,@(mapcar '((a b)
                     $(= ,a ,b))
                  (@ car inits)
                  (@ cadr inits))
        ,@body)
       ,(dup nil (length inits)))
     $(progn
        ,@body)))
