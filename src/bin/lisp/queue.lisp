(fn make-queue ()
  (cons nil nil))

(fn enqueue (x . vals)
  (setcar x (cdr (setcdr (or (car x) x) vals)))
  vals)

(fn queue-list (x)
  (car x))
