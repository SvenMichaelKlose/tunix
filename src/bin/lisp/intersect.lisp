(load "defsetfn.lisp")

(defsetfn intersect
  ;"Elements that are in both lists."
  (and a b
       (? (member (car a) b)
          (cons (car a) (intersect (cdr a) b))
          (intersect (cdr a) b))))
