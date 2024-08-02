(fn copy-tree (x)
  ;"Copy tree."
  (? (cons? x)
     (cons (copy-tree (car x))
           (copy-tree (cdr x)))
     x))

(message "Testing COPY-TREE...")
(or (equal (copy-tree '((1 2) (3 (4 5))))
           '((1 2) (3 (4 5))))
    (error))
