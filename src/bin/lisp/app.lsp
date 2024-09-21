(fn app (name)
  ;"Mark start or end of app."
  (push name *universe*)
  (push (list name) *macros*))

(fn %err-app (name)
  ;"Issue marking error."
  (error "Invalid app " name "."))

(fn %chk-app (name l)
  ;"Check if app start and end both have been marked."
  (!= (member name l)
    (or ! (%err-app name))
    (or (member name (cdr !))
        (or ! (%err-app name)))))

(fn rm-app (name)
  ;"Remove previously marked app."
  (%chk-app name *universe*)
  (!= (split name *universe*)
    (= *universe* (nconc (car !) (caddr !))))
  (%chk-app name *macros*)
  (%rm-app name '*universe*)
  (%rm-app name '*macros*))
