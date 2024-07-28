(load "defsetfn.lisp")

(defsetfn subseq?
  ;"Check if list a is a subset of list b."
  (every $((x) (member x ,a)) b))
