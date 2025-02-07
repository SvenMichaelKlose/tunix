(fn ensure-list (x)
  (? (atom x)
     (.. x)
     x))
