(macro with (inits . body)
  ;"Local symbol values."
  (? inits
     (((i)
        $((,(@ car i)
           ,@body)
          ,@(@ cadr i)))
      (group2 inits))
     $(progn
        ,@body)))
