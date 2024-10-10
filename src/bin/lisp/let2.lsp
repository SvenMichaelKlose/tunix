(macro let* (inits . body)
  ;"Local symbol values."
  (? inits
     (((i)
        $((,(@ car i)
           ,@(@ '((x) (cons '= x)) i)
           ,@body)
          ,@(dup nil (length i))))
      (group2 inits))
     $(progn
        ,@body)))
