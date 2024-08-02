(fn make-queue ()
  (cons nil nil))

(fn enqueue (x . vals)
  (setcar x (cdr (setcdr (or (car x) x) vals)))
  vals)

(fn queue-list (x)
  (cdr x))

(message "Testing ENQUEUE...")
(or (equal (let q (make-queue)
             (enqueue q 42)
             (enqueue q 23)
             (queue-list q))
           '(42 23))
    (error))
