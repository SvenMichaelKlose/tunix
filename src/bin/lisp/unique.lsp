(fn unique (x)
  ;"Unique elements of a list."
  (and x
       (? (member (car x) (cdr x))
          (unique (cdr x))
          (. (car x) (unique (cdr x))))))
