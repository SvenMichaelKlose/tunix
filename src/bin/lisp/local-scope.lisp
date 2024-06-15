(macro let (n v . body)
  "Introduce local variable."
  $(((,n)
      ,@body
    ,v)))

(macro with (inits . body)
  "Introduce local variables."
  $(((,(carlist inits))
      ,@body)
    ,@(cdrlist inits)))
