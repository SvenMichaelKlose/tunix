(fn ls ()
  (awhen (opendir)
    (with-queue q
      (awhile (readdir !)
        (enqueue q !))
      (closedir !))))
