(macro dolist (iter . body)
  ;"Loop over conses of a list."
  (with ((v       (car iter))
         (init    (cadr iter))
         (result  (cddr iter))
         (i       (symbol))
         (tag     (symbol)))
    $(let ,i ,init
       (block nil
         ,tag
         (or ,i (return ,(car result)))
         (= ,v (car ,i))
         ,@body
         (= ,i (cdr ,i))
         (go ,tag)))))

(print 'dolist)(terpri)
(eval
  (print
    (macroexpand
      '(dolist (i '(1 2 3 4 5 6 7 8 9 10))
         (terpri)(out '"Item ")(print i)))))
(terpri)
