(macro let (n v . body)
  ;"Local symbol value."
  $(((,n)
      ,@body)
    ,v))
