(macro when (cond . body)
  $(? ,cond
      (progn
        ,@body)))
