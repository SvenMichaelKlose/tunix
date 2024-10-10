(macro with-in (v x . body)
  ;"Evaluate BODY with input channel V provided by X. Return error or NIL."
  (let g (symbol)
    $(with (,g fnin
            ,v ,x)
       (unless ,v
         (return (or (err) t)))
       (setin ,v)
       ; TODO: Replace this with an interpreter-level
       ; UNWIND-PROTECT implementation to ensure proper
       ; cleanup, no matter what RETURN scenario.
       (block nil
         ,@body)
       (prog1 (err)
         (setin ,g)
         (close ,v)))))
