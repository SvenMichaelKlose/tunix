(macro while (test . body)
  $(do ()
       ((not ,test))
     ,@body))
