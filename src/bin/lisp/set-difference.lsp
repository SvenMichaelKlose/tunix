(defsetfn set-difference
  ;"Elements in list b that are not in list a."
  (and b
       (? (member (car b) a)
          (set-difference a (cdr b))
          (cons (car b) (set-difference a (cdr b))))))
