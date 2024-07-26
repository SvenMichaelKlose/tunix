(macro dotimes (init . body)
  (with ((i    (car init))
         (n    (cadr init))
         (g    (symbol))
         (stag (symbol))
         (etag (symbol)))
    $(let ,g ,n
       (block nil
         (= ,i ,g)
         stag
         (= ,i (-- ,i))
         (? (<= ,i 0)
            (go etag))
         ,@body
         (go stag)
         etag))))

(message "Testing DOTIMES...")
(dotimes (i 10)
  (print i))
