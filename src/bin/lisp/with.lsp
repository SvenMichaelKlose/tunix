(macro with (inits . body)
  ;"Local symbol values."
  (? inits
     $((,(@ car (group2 inits))
        ,@body)
       ,@(@ cadr (group2 inits)))
     $(progn
        ,@body)))
