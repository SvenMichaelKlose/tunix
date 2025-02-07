(fn copy-list (x)
  (with-queue q
    (dolist (i x)
      (enqueue q i))))
