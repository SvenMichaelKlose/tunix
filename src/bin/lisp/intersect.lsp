(fn intersect (a b)
  ;"Elements that are in both lists."
  (and a b
       (? (member (car a) b)
          (. (car a) (intersect (cdr a) b))
          (intersect (cdr a) b))))
