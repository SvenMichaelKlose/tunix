(in-package 'c/ce '(call))

(fn call (x)
  (fn argexpand (d v)
    (and (not d) v
      (error "Too many args: " v))
    (when d
      (unless v
        (error "Missing arg: " d))
      (? (and d (atom d))
         $(.. ,@v)
         $(,v. ,@(argexpand .d .v)))))
  (!? (*fi*.argdef x.)
      $(,x. ,@(argexpand ! .x))
      $(*> ,x. (.. ,@.x))))

(walker c/callexpand (x)
  (and (%=? x)
       (cons? ..x.))
     $(%= ,.x.  (call ..x.)))

(in-package nil)
