(or (macro? 'defsetfn)
    (load "defsetfn.lisp"))
(or (cons? every)
    (load "every.lisp"))

(defsetfn subseq?
  ;"Check if list a is a subset of list b."
  (every $((x) (member x ,a)) b))
