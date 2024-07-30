(or (macro? 'dolist)
    (load "dolist.lisp"))
(or (macro? 'with-queue)
    (load "with-queue.lisp"))

(fn remove-if (f x)
  (with-queue q
    (dolist (i x (queue-list q))
      (or (funcall fun i)
          (enqueue q i)))))

