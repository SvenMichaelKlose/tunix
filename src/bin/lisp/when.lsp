(macro when (cond . body)
  $(? ,cond
      ((()
         ,@body))))
