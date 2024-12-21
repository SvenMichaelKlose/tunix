(fn enqueue (x . vals)
  (setcar x (cdr (setcdr (or (car x) x) vals)))
  vals)
