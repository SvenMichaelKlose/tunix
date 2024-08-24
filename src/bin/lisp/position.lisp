(fn position (x l . f)
  (= f (? f (car f) eql))
  (and l
       (? (f x (car l))
          0
          (!? (position x (cdr l) f)
              (++ !)))))
