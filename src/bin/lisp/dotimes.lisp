(macro dotimes (init . body)
  (with ((i    (car init))
         (n    (cadr init))
         (r    (cddr init))
         (tag  (symbol)))
    $(let ,i ,n
       (block nil
         ,tag
         (= ,i (-- ,i))
         (? (< ,i 0)
            (return ,(car r)))
         ,@body
         (go ,tag)))))

(message "Testing DOTIMES...")
(dotimes (i 10 (terpri))
  (print i))
