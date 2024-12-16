(macro assert (cond . msg)
  $(or ,cond
       (funcall error ,@msg)))
