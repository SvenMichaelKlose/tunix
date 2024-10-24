(fn copy-alist (x)
  (with-queue q
    (dolist (i x)
      (enqueue q (cons (car i) (cdr i))))))
