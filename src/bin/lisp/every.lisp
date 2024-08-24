(fn every (f x)
  "F true for all in X?"
  (do ((i x (cdr i)))
      ((not i) t)
    (or (f (car i))
        (return nil))))
