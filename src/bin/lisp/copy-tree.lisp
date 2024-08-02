(fn copy-tree (x)
  ;"Copy tree."
  (? (cons? x)
     (cons (copy-tree (car x))
           (copy-tree (cdr x)))
     x))

(message "Smoke-testing COPY-TREE...")
(print (copy-tree '((1 2) (3 (4 5)))))
