(macro dolist (iter . body)
  ;"Loop over conses of a list."
  (let i (symbol)
    $(do ((,i ,(cadr iter) (cdr ,i)))
         ((not ,i) ,@(cddr iter))
       (let ,(car iter) (car ,i)
         ,@body))))

(message "Testing DOLIST...")
(dolist (i '(1 2 3 4 5 6 7 8 9 10) (terpri))
  (terpri)(out '"Item ")(print i))
