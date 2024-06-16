(macro while (cond result . body)
  ;"Evaluate BODY while COND is true and return RESULT."
  (let tag (string)
    $(block nil
       ,tag
       (or ,cond
           (return))
       ,@body
       (go ,tag))))
