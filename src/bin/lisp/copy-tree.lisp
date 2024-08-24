(fn copy-tree (x)
  ;"Copy tree."
  (? (cons? x)
     (cons (copy-tree (car x))
           (copy-tree (cdr x)))
     x))
