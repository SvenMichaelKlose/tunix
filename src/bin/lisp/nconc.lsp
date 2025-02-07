(or (builtin? nconc)
  (fn nconc lists
    (do ((result nil)
         (tail   nil)
         (x      lists (cdr x)))
        ((not x) result)
      (and (car x)
           (? result
              ((()
                 (=-cdr tail (car x))
                 (= tail (last (car x)))))
              ((()
                (= result (car x))
                (= tail (last result)))))))))
