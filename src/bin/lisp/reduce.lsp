(fn reduce (f l . init)
  (? l
     (!= (!? (car init)
             (f ! (car l))
             (car l))
       (dolist (i (cdr l) !)
         (= ! (f ! i))))
     init))
