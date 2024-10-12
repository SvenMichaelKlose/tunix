(macro unless (cond . body)
  $(? (not ,cond)
      ((()
         ,@body))))
