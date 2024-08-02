(fn max n
  ;"Maximum of n."
  (? (not n)
     (error "No args")
     (let result (car n)
       (dolist (n (cdr n) result)
         (? (> n result)
            (= result n))))))
