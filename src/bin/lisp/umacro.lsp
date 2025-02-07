(var *umacros* nil)

(macro umacro (grp n a . body)
  (print $(umacro ,grp ,n ,a))
  (!? (assoc grp *umacros*)
      (acons! n (. a body) !)
      (acons! grp (. n (. a body)) *umacros*))
  nil)

(fn umacroexpand (grp x)
  (with-global *macros* (cdr (assoc grp *umacros*))
    (macroexpand x)))
