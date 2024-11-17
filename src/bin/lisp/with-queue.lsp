(macro with-queue (q . body)
  ;"Evaluate block with queue Q and return its list."
  $(let (,q (. nil nil))
     (block nil
       ,@body
       (cdr ,q))))
