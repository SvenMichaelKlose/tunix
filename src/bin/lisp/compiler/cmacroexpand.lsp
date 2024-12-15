(load 'compiler/package.lsp)
(require 'let 'prog1 'push 'pop '!= 'aif 'with-global 'with-in 'with-out 'when 'awhen 'acons! 'while 'group 'mapcar 'mapcan)

(var *cmacros* nil)

(macro defcm (n a . body)
  (print $(defcm ,n ,a))
  (acons! n (. a (@ macroexpand body))
          *cmacros*)
  nil)

(fn mklogical (which)
  (let (end "")
    $(%block
       ,@(mapcan $((x)
                    (list x '(,which ,end)))
                 (butlast x))
       ,@(last x)
       (%tag ,end))))

(defcm and x
  (mklogical '%go-nil))

(defcm or x
  (mklogical '%go-nnil))

(fn mkif (end x)
  (? .x
     (let (next "")
       $(,x.
         (%go-nil ,next)
         ,.x.
         (%go ,end)
         (%tag ,next)))
     (list x.)))

(defcm ? x
  (let (end "")
    $(%block
       ,@(mapcan $((x)
                    (mkif ,end x))
                 (group x 2))
       (%tag ,end))))

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

(defcm block (n . body)
  (with-global *macros* (list (. 'block cmblock)
                              (. 'return cmreturn))
    (apply cmblock n body)))

(fn cmacroexpand (x)
  (with-global *macros* *cmacros*
    (macroexpand x)))

(in-package nil)
