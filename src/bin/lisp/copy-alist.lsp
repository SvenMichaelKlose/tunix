(fn copy-alist (x)
  (with-queue q
    (dolist (i x)
      (enqueue q (. (car i) (cdr i))))))
