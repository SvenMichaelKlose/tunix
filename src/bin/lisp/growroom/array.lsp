; Binary tree based arrays.

(fn make-array (n))

; array := (root . bits)
(fn %^ (a n bits)
  (? (== 1 bits)
     (? (== 0 (bit-and n 1))
        (car a)
        (cdr a))
     (%^ (? (== 0 (bit-and n 1))
            (car a)
            (cdr a))
         (>> n 1) (-- bits))))

(fn ^ (a n)
  (%^ (car a) n (cdr a)))

(fn %=^ (v a n bits)
  (? (== 1 bits)
     (? (== 0 (bit-and n 1))
        (setcar a v)
        (setcdr a v))
     (%=^ (? (== 0 (bit-and n 1))
             (car a)
             (cdr a))
          (>> n 1) (-- bits))))

(fn =^ (v x n)
  (%=^ v (car a) n (cdr a)))
