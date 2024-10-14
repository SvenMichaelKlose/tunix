(macro with-queue (q . body)
  ;"Evaluate block with queue Q and return its list."
  $(let (,q (make-queue))
     (block nil
       ,@body
       (queue-list ,q))))
