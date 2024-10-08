(macro with* (inits . body)
  ;"Local symbol values."
  (? inits
     $((,(@ car inits)
        ,@(mapcar '((a b)
                     $(= ,a ,b))
                  (@ car inits)
                  (@ cadr inits))
        ,@body)
       ,(aprog1 nil
          (dotimes (i (length inits))
            (push nil !))))
     $(progn
        ,@body)))
