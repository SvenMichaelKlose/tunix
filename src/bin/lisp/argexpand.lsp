; See doc/compiler.md

(fn argexpand (def val)
  (with-queue q
    (do ((d def (cdr d))
         (v val (cdr v)))
        ((and (not d)
              (? v
                 (error "Too many arguments: " v)
                 t)))
      (? (atom d)
         (return (enqueue q (list d v)))
         (? v
            (enqueue q (list (car d) (car v)))
            (error "Argument " d " missing"))))))
