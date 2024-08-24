(fn split (x l . f)
  (= f (? f (car f) eql))
  (and l
       (!? (position x l f)
           (cons (subseq l 0 !)
                 (split x (subseq l (++ !)) f))
           (list l))))
