(fn make-queue ()
  (cons nil nil))

(fn enqueue (x . vals)
  (setcar x (cdr (setcdr (or (car x) x) vals)))
  vals)

(fn queue-pop (x)
  (prog1 (cadr x)
    (or (setcdr x (cddr x))
        (setcar x nil))))

(fn queue-list (x)
  (car x))

(fn ensure-list (x)
  (? (atom x)
     (list x)
     x))

(macro with-queue (q . body)
  $(with ,(@ '((x)
                $(,x (make-queue)))
             (ensure-list q))
     ,@body))
