(macro when (cond . body)
  $(? ,cond
      (progn
        ,@body)))

(macro unless (cond . body)
  $(? ,cond
      nil
      (progn
        ,@body)))
