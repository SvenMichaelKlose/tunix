(fn position-if (f l)
  (do ((n 0 (++ n))
       (i l .i))
      ((not i))
    (? (f i.)
       (return n))))
