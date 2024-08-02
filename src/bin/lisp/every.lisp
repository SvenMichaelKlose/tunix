(fn every (f x)
  "F true for all in X?"
  (do ((i x (cdr i)))
      ((not i) t)
    (or (f (car i))
        (return nil))))

(message "Testing EVERY...")
(or (every '((x) (== x 1)) '(1 1 1 1))
    (error))
(and (every '((x) (== x 1)) '(1 1 2 1))
    (error))
