(fn argnameexpand (def)
  (with-queue q
    (do ((d def (and (cons? .d) .d)))
        ((not d) .q)
      (enqueue q (? (atom d) d d.)))))
