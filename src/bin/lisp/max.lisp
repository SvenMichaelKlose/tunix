(fn max n
  ;"Maximum of n."
  (? (not n)
     (error "No args")
     (let result (car numbers)
       (dolist (n (cdr n) result)
         (? (> n result)
            (= result n))))))
