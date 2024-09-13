(macro with (inits . body)
  ;"Local symbol values."
  (? inits
     $((,(carlist inits)
        ,@body)
       ,@(carlist (cdrlist inits)))
     $(progn
        ,@body)))
