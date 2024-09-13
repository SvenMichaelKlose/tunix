(macro unless (cond . body)
  $(? (not ,cond)
      (progn
        ,@body)))
