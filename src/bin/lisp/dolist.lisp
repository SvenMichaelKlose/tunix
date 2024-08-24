(macro dolist (iter . body)
  ;"Loop over conses of a list."
  (let i (symbol)
    $(do ((,i ,(cadr iter) (cdr ,i)))
         ((not ,i) ,@(cddr iter))
       (let ,(car iter) (car ,i)
         ,@body))))
