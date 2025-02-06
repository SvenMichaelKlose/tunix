(in-package 'c/ce '(call))

(fn call (x)
  (fn args (d v)
    (and (not d) v
      (error "Too many args: " v))
    (when d
      (unless v
        (error "Missing arg: " d))
      (? (and d (atom d))
         $(.. ,@v)
         $(,v. ,@(args .d .v)))))
  (!? (*fi*.argdef x.)
      $(,x. ,@(args ! .x))
      $(*> ,x. (.. ,@.x))))

(walker compiler/callexpand (x)
  (%=? x)
    $(%= ,.x.
         ,(? (cons? ..x.)
             (call ..x.)
             ..x.)))

(in-package nil)
