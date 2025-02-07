(fn set-difference (a b)
  ;"Elements in list b that are not in list a."
  (and b
       (? (member b. a)
          (set-difference a .b)
          (. b. (set-difference a .b)))))
