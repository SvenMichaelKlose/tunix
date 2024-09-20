(fn mark-app (name)
  ;"Mark start or end of app."
  (push name *universe*))

(fn %err-app (name)
  ;"Issue marking error."
  (error "Invalid app " name "."))

(fn %chk-app (name l)
  ;"Check if app start and end both have been marked."
  (!= (member name l)
    (or ! (err-app name))
    (or (member name (cdr !))
        (or ! (err-app name)))))

(fn %rm-app (name s)
  ;"Remove app between its marks in list of symbol S."
  (!= (split name (symbol-value s))
    (set s (car !) (caddr !))))

(fn rm-app (name)
  "Remove previously marked app."
  (%chk-app name *universe*)
  (%chk-app name *macros*)
  (%rm-app name *universe*)
  (%rm-app name *macros*))
