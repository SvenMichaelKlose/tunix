(in-package 'c/be
  '(*blocks* cblock creturn))

(var *blocks* nil)

; TODO: Collect potential tags.
(fn cblock (n . body)
  (let (end "")
    (push (. n end) *blocks*)
    (!= (@ macroexpand body)
      (pop *blocks*)
      $(%block
         ,@!
         (%tag ,end)))))

(fn creturn (v . n)
  (!? (cdr (assoc n *blocks*))
      $(%block
         ,v
         (%go ,!))
      (error "Unknown BLOCK " n)))

(umacro block block (n . body)
  (with-global
      *macros* (.. (. 'block  cblock)
                   (. 'return creturn))
    (apply cblock n body)))

(fn compiler/blockexpand (x)
  (umacroexpand 'block x))

(in-package nil)
