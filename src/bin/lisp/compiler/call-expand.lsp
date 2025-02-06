(fn call (x)
  (fn r (d v)
    (and (not d) v
      (error "Too many args: " v))
    (when d
      (unless v
        (error "Missing arg: " d)))
      (? (and d (atom d))
         $(.. ,@v)
         (. v. (r .d .v))))
  (!? (*fi*.argdef x.)
      (. x. (r ! .x))
      $(*> ,x. (.. ,@.x))))

(walker callexpand (x)
  (%=? x)
    $(%= ,.x.
         ,(? (cons? ..x.)
             (call ..x.)
             ..x.)))
