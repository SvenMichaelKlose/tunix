(fn pad (i x)
  (? (and (cons? x)
          (cdr x))
     (cons (car x) (cons i (pad i (cdr x))))
     x))

(print (pad 'gap '(1 2 3)))
