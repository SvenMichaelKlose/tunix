(macro let-when (v x . body)
  $(let ,v ,x
     (when ,v
       ,@body)))
