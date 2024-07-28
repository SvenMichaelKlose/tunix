(fn copy (x)
  ;"Copy tree."
  (? (cons? x)
     (cons (car x) (cdr x))
     x))
