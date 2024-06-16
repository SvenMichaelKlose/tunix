(macro with (inits . body)
  ;"Local symbol values."
  $(((,(carlist inits))
      ,@body)
    ,@(cdrlist inits)))
