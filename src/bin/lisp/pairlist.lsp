(fn pairlist (a b)
  (with-queue q
    (do ((i a (cdr i))
         (j b (cdr j)))
        ((or (not i) (not j)))
      (enqueue q (cons (car i) (car j))))))
