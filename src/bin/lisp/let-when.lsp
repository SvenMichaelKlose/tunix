(fn let-when (v cond . body)
  $(let ,v ,cond
     (when ,v
       ,@body)))
