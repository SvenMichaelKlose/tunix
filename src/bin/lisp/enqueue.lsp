(fn enqueue (x . vals)
  (setcar x (cdr (setcdr (or x. x) vals)))
  vals)
