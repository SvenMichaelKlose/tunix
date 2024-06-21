(macro dolist (iter . body)
  ;"Loop over conses of a list."
  (with ((v       (car iter))
         (init    (cadr iter))
         (result  (cddr iter))
         (tag     (symbol)))
    $(let ,v ,init
       (block nil
         (or ,v (return ,(car result)))
         ,tag
         ,@body
         (= ,v (cdr ,v))
         (go ,tag)))))
