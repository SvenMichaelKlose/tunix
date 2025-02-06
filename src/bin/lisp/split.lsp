(fn split (x l . f)
  (= f (? f (car f) eql))
  (and l
       (!? (position x l f)
           (. (subseq l 0 !)
              (split x (subseq l (++ !)) f))
           (.. l))))
