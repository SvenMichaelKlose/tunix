(macro with (inits . body)
  ;"Local symbol values."
  (? inits
     $((,(@ car inits)
        ,@body)
       ,@(@ car (@ cdr inits)))
     $(progn
        ,@body)))
