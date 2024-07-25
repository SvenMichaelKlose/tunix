(macro progn body
  ;"Return value of last expression."
  $(block t ,@body))

(out "Testing PROGN...")(terpri)
(or (equal (macroexpand '(progn (error)))
           '(block t (error))))
