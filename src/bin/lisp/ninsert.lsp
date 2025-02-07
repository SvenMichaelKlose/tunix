(fn ninsert (l n x)
  (nconc (subseq l 0 n)
         (. x (nthcdr n l))))
