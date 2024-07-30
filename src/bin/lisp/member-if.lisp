(or (macro? 'do)
    (load "do.lisp"))

(fn member-if (f x)
  (do ((i x (cdr i)))
       ((not i))
    (? (f (car i))
       (return i))))
