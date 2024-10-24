(fn mapcar (f . l)
  (and (car l)
       (. (apply f (@ car l))
          (apply mapcar f (@ cdr l)))))
