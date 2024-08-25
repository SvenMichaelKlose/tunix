(fn mapcar (f . l)
  (and (car l)
       (cons (apply f (carlist l))
             (apply mapcar f (cdrlist l)))))
