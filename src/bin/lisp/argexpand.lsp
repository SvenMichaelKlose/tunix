; See doc/compiler.md

(fn argexpand (def val)
  (with-queue q
    (do ((d def .d)
         (v val .v))
        ((and (not d)
              (? v
                 (error "Too many arguments: " v)
                 t)))
      (? (atom d)
         (return (enqueue q (. d v)))
         (? v
            (enqueue q (. d. v.))
            (error "Argument " d " missing"))))))
