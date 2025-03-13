(macro walker (n x)
  $(def-filter ,n ,x
     ,@body
     (atom ,x) ,x
     (named-lambda? ,x)
       (with-temporary
           *fi* (lambda-fi ,x)
         (,name (lambda-body ,x)))
     (. (car ,x) (,name (cdr ,x)))))
