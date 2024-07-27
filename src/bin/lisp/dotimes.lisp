(macro dotimes (init . body)
  (with ((i    (car init))
         (n    (cadr init))
         (r    (cddr init))
         (g    (symbol))
         (tag  (symbol)))
    $(let ,g ,n
       (block nil
         (= ,i ,g)
         ,tag
         (= ,i (-- ,i))
         (? (< ,i 0)
            (return ,(car r)))
         ,@body
         (go ,tag)))))

(message "Testing DOTIMES...")
(dotimes (i 10)
  (print i))
