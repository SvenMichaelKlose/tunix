(macro when (cond . body)
  "Evaluate block when..."
  $(? ,x (progn ,@body)))

(macro unless (x . body)
  "Evaluate block unless..."
  $(? ,x nil (progn ,@body)))
