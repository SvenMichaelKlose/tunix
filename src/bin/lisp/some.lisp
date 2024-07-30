(fn some (f x)
  "F true for some in X?"
  (do ((i x (cdr i)))
      ((not i) nil)
    (or (f i)
        (return t))))
