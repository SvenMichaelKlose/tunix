(or (builtin? nconc)
  (fn nconc lists
    (do ((result nil)
         (tail nil)
         (x lists (cdr x)))
        ((not x) result)
      (and (car x)
           (? result
              (progn
                (setcdr tail (car x))
                (= tail (last (car x))))
              (progn
                (= result (car x))
                (= tail (last result))))))))
