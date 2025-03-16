(fn remove-if-not (f x)
  (remove-if $((x)
                (not (,f x)))
             x))
