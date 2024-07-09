(macro dotimes (init . body)
  (with ((i    init.)
         (n    .init.)
         (g    (symbol))
         (stag (symbol))
         (etag (symbol)))
    $(let ,g ,n
       (block nil
         (= ,i ,g)
         stag
         (? (<= ,i 0)
            (go etag))
         ,@body
         (go stag)
         etag))))

(print 'dotimes)(terpri)
(dotimes (i 10) (print i))
