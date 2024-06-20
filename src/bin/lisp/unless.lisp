(macro unless (cond . body)
  $(? ,cond
      nil
      (progn
        ,@body)))
