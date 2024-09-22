(fn app (name)
  ;"Mark start or end of app."
  (push name *universe*)
  (push (list name) *macros*))

(fn rm-app (name)
  (= *universe* (!= (split name *universe*)
                  (or (caddr !) (error))
                  (nconc (car !) (caddr !))))
  (= *macros* (!= (split-if $((x)
                               (eq (car x) ',name))
                            *macros*)
                  (or (caddr !) (error))
                  (nconc (car !) (caddr !)))))
