(fn aremove-if (f l)
  (with-queue q
    (do ((l l (cdr l)))
        ((not l))
      (unless (f (car l))
        (enqueue q (car l))))))
