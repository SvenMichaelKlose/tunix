(fn unique (x)
  ;"Unique elements of a list."
  (and x
       (? (member x. .x)
          (unique .x)
          (. x. (unique .x)))))
