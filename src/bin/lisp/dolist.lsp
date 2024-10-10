(macro dolist (iter . body)
  ;"Loop over conses of a list."
  (with (i (symbol))
    $(do ((,i ,(cadr iter) (cdr ,i)))
         ((not ,i) ,@(cddr iter))
       (with (,(car iter) (car ,i))
         ,@body))))
