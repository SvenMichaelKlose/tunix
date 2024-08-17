(macro awhen (cond . body)
  $(let ! ,cond
     (when !
       ,@body)))
