(fn nthcdr (n l)
  (? (or (not l)
         (== 0 n))
     l
     (nthcdr (-- n) (cdr l))))
