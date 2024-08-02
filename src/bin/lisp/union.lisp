(or (cons? unique)
    (load "unique.lisp"))
(or (macro? 'defsetfn)
    (load "defsetfn.lisp"))

(defsetfn union
  ;"Unique elements from both lists."
  (unique (append a b)))

(message "Testing UNION...")
(or (equal (union '(l l i i) '(s s p p))
           '(l i s p))
    (error))
