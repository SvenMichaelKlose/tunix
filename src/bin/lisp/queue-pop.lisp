(fn queue-pop (x)
  (prog1 (cadr x)
    (or (setcdr x (cddr x))
        (setcar x nil))))
