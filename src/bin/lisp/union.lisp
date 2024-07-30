(or (cons? unique)
    (load "unique.lisp"))
(or (macro? 'defsetfn)
    (load "defsetfn.lisp"))

(defsetfn union
  ;"Unique elements from both lists."
  (unique (append a b)))
