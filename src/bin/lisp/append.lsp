(or (builtin? append)
  (fn append x
    ;"Copy and concatenate."
    (?
      (car x)
        (cons (caar x)
              (apply append (cdar x) (cdr x)))
      (cdr x)
        (apply append (cdr x)))))
