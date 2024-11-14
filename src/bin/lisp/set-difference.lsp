(fn set-difference (a b)
  ;"Elements in list b that are not in list a."
  (and b
       (? (member (car b) a)
          (set-difference a (cdr b))
          (. (car b) (set-difference a (cdr b))))))
