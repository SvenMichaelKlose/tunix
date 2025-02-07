(macro assert (cond . msg)
  $(unless ,cond
     (funcall out ,@msg)))
