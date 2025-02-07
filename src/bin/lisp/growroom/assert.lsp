(var *a?* t)

(macro assert (x . msg)
  (when *a?*
    $(or ,x (error ,@(or msg (list (list x)))))))
