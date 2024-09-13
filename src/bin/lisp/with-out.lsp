(macro with-out (v x . body)
  ;"Evaluate BODY with input channel V provided by X. Return error or NIL."
  (let g (symbol)
    $(with ((,g fnout)
            (,v ,x))
       (? (not ,v)
          (or (err) t)
          (progn
            (setout ,v)
            ; TODO: Replace this with an interpreter-level
            ; UNWIND-PROTECT implementation to ensure proper
            ; cleanup, no matter what RETURN scenario.
            (block nil
              ,@body)
            (prog1 (err)
              (setout ,g)
              (close ,v)))))))
