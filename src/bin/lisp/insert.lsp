(fn insert (l n x)
  (append (subseq l 0 n)
          (. x (nthcdr n l))))
