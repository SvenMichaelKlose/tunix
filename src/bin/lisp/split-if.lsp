(fn split-if (f l)
  (and l
       (!? (position-if f l)
           (. (subseq l 0 !)
              (split-if f (subseq l (++ !))))
           (list l))))
