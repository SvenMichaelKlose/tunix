(macro progn body
  ;"Return value of last expression."
  $(block t ,@body))

(message "Testing PROGN...")
(or (equal (macroexpand '(progn (error)))
           '(block t (error))))
