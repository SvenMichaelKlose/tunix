(fn dup (x n)
  (aprog1 nil
    (dotimes (i n)
      (push x !))))
