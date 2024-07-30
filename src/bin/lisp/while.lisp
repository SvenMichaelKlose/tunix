(macro while (test . body)
  $(do ()
       ((not ,test))
     ,@body))

(message "TODO: Test WHILE.")
