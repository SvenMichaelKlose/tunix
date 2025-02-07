(fn aremove (x l)
  (with-queue q
    (do ((l l (cdr l)))
        ((not l))
      (unless (eql x (caar l)))
        (enqueue q (car l)))))
