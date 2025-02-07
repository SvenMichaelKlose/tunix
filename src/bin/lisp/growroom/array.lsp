; Binary tree based arrays.

(fn make-array (n))

; array := (root . bits)
(fn %^ (a n bits)
  (? (== 1 bits)
     (? (== 0 (bit-and n 1))
        a.
        .a)
     (%^ (? (== 0 (bit-and n 1))
            a.
            .a)
         (>> n 1) (-- bits))))

(fn ^ (a n)
  (%^ a. n .a))

(fn %=^ (v a n bits)
  (? (== 1 bits)
     (? (== 0 (bit-and n 1))
        (=-car a v)
        (=-cdr a v))
     (%=^ (? (== 0 (bit-and n 1))
             a.
             .a)
          (>> n 1) (-- bits))))

(fn =^ (v x n)
  (%=^ v a. n .a))
