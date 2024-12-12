(fn copy-tree (x)
  ;"Copy tree."
  (? (cons? x)
     (. (copy-tree x.)
        (copy-tree .x))
     x))
