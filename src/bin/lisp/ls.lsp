(require 'awhen 'awhile 'with-queue 'enqueue 'queue-list)

(fn ls ()
  (awhen (opendir)
    (with-queue q
      (awhile (readdir !)
        (enqueue q !))
      (closedir !))))
