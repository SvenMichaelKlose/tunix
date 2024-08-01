(or (cons? remove-if)
    (load "remove-if.lisp"))
(or (cons? intersect)
    (load "intersect.lisp"))
(or (macro? 'defsetfn)
    (load "defsetfn.lisp"))

(defsetfn set-exclusive-or
  ;"Elements that are not in both lists."
  (with ((subset  (intersect a b))
         (pred    $((x)
                    (member x ,subset))))
    (append (remove-if pred a)
            (remove-if pred b))))
