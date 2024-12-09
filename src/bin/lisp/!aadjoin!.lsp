(macro !aadjoin! (key init new-cdr al)
  $(!? (assoc key al)
       new-cdr
       (acons! key init al)))
