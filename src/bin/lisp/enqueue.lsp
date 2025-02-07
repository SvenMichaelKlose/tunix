(fn enqueue (x . vals)
  (=-car x (cdr (=-cdr (or x. x) vals)))
  vals)
