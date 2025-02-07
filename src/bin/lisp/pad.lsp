(fn pad (i x)
  (? (and (cons? x) .x)
     (. x. (. i (pad i .x)))
     x))
