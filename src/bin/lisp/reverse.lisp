(or (macro? 'push)
    (load "stack.lisp"))

(fn reverse (x)
  (do ((l)
       (i x (cdr i)))
      ((not i) l)
    (push (car i) l)))

(message "Testing REVERSE...")
(or (equal (reverse '(p s i l))
           '(l i s p))
    (error))
