(macro progn body
  ;"Return value of last expression."
  $(block t ,@body))

(print 'macroexpand-progn)(terpri)
(or (equal (macroexpand '(progn (error)))
           '(block t (error))))
