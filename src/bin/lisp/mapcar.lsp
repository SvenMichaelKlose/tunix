(fn mapcar (f . l)
  (and (car l)
       (cons (apply f (@ car l))
             (apply mapcar f (@ cdr l)))))
