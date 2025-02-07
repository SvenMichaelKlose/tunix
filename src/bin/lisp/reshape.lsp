(fn reshape (l . dims)
  ; "Reshape L to multiple dimensions."
  (? .dims
     (@ $((x)
           (!? (cdr ',dims)
               (apply reshape x !)
               x))
        (group l (reduce * dims 1)))
     l))

(print (reshape '(1 2 3 4 5 6 7 8) 2 3))
