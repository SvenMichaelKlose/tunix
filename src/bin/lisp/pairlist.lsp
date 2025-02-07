(fn pairlist (a b)
  (with-queue q
    (do ((i a .i)
         (j b .j))
        ((or (not i) (not j)))
      (enqueue q (. i. j.)))))
