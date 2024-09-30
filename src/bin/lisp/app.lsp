(or (cons? require)
    (load "require.lsp"))

(require '!= 'push 'do 'mapcar 'position 'position-if 'split 'split-if)

(var *a* nil)

(fn app (name)
  ;"Mark start or end of app."
  (= *a* name)
  (push name *universe*)
  (push (list name) *macros*))

(fn reset-app ()
  (= *universe* (member *a* *universe*))
  (= *macros* (member-if $((x)
                            (eq (car x) ',*a*))
                         *macros*)))
(fn rm-app (name)
  (= *universe* (!= (split name *universe*)
                  (or (caddr !) (error))
                  (dolist (i (cadr !))
                    (= i i))
                  (nconc (car !) (caddr !))))
  (= *macros* (!= (split-if $((x)
                               (eq (car x) ',name))
                            *macros*)
                (or (caddr !) (error))
                (nconc (car !) (caddr !)))))
