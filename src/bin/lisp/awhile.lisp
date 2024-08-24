(macro awhile (test . body)
  $(let ! nil
     (do ()
         ((not (= ! ,test)))
       ,@body)))
