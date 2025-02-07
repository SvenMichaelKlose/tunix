(fn queue-pop (x)
  (prog1 (cadr x)
    (or (=-cdr x (cddr x))
        (=-car x nil))))
