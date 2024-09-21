(or (builtin? nthcdr)
    (fn nthcdr (n l)
      (and (<= 0 n)
        (? (or (not l)
               (== n 0))
           l
           (nthcdr (-- n) (cdr l))))))
