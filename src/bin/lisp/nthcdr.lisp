(fn nthcdr (n l)
  ;"Return the nth cdr of LIST, or nil if n is greater than the length of the list."
  (? (or (not l) (== 0 n))
      l
      (nthcdr (-- n) (cdr l))))
