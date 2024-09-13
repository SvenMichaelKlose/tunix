(fn remove-if (f x)
  (with-queue q
    (dolist (i x (queue-list q))
      (or (funcall fun i)
          (enqueue q i)))))

