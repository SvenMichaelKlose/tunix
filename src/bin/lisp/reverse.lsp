(fn reverse (x)
  (do ((l)
       (i x (cdr i)))
      ((not i) l)
    (push (car i) l)))
