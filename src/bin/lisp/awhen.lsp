(macro awhen (cond . body)
  $(((!)
      (and !
        ((()
           ,@body))))
    ,cond))
