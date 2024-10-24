(fn copy-tree (x)
  ;"Copy tree."
  (? (cons? x)
     (. (copy-tree (car x))
        (copy-tree (cdr x)))
     x))
