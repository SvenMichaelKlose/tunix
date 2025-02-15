(require 'let 'prog1 'push 'pop '!=
         'aif 'with-global 'with-in
         'with-out 'when 'awhen 'acons!
         'while 'group 'mapcar '+@
         'umacro)

(in-package 'c/cm
  '(mklogical mkif *blocks*))

(fn mklogical (which)
  (let (end "")
    $(%block
       ,@(+@ $((x)
                (.. x '(,which ,end)))
             (butlast x))
       ,@(last x)
       (%tag ,end))))

(umacro compiler and x
  (mklogical '%go-nil))

(umacro compiler or x
  (mklogical '%go-nnil))

(fn mkif (end x)
  (? .x
     (let (next "")
       $(,x.
         (%go-nil ,next)
         ,.x.
         (%go ,end)
         (%tag ,next)))
     (.. x.)))

(umacro compiler ? x
  (let (end "")
    $(%block
       ,@(+@ $((x)
                (mkif ,end x))
             (group x 2))
       (%tag ,end))))

(var *b* nil)

(umacro compiler block (n . body)
  (let (end "")
    (acons! n end *b*)
    (!= (macroexpand body)
      (pop *b*)
      $(%block
         ,@!
         (%tag ,end)))))

(umacro compiler return (v . n)
  (!? (cdr (assoc n *b*))
      $(%block
         ,v
         (%go ,!))
      (error "Unknown BLOCK " n)))

(fn compiler/cmacroexpand (x)
  (umacroexpand 'compiler x))

(in-package nil)
