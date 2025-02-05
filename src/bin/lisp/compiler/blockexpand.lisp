(in-package 'compiler/blockexpand
  '(cmblock cmreturn))

(var *blocks* nil)

; TODO: Collect potential tags.
(fn cmblock (n . body)
  (let (end "")
    (push (. n end) *blocks*)
    (!= (@ macroexpand body)
      (pop *blocks*)
      $(%block
         ,@!
         (%tag ,end)))))

(fn cmreturn (v . n)
  (!? (cdr (assoc n *blocks*))
      $(%block
         ,v
         (%go ,!))
      (error "Unknown BLOCK " n)))

(umacro block block (n . body)
  (with-global
      *macros* (list (. 'block cmblock)
                     (. 'return cmreturn))
    (apply cmblock n body)))

(fn blockexpand (x)
  (umacroexpand 'block x))

(in-package nil)
