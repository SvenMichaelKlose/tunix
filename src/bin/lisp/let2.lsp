(macro let* (inits . body)
  ;"Local symbol values."
  (? inits
     (((i)
        $((,(@ car i)
           ,@(@ '((x) (. '= x)) i)
           ,@body)
          ,@(dup nil (length i))))
      (group2 inits))
     $((()
         ,@body))))
