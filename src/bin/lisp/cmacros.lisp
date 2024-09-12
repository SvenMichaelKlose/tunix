(load "let.lisp")
(load "!=.lisp")
(load "mapcar.lisp")
(load "with-global.lisp")

(var *cmacros* nil)

(macro defcm (n a . body)
  (print $(defcm ,n ,a))
  (push (cons n (cons a (@ macroexpand body)))
        *cmacros*)
  nil)

(fn mkgo (which end)
  $((x)
     (list x
           '(,which ,end))))

(fn mklogical (which)
  (let end ""
    $(%block
       ,@(mapcan (mkgo which end)
                 (butlast x))
       ,@(last x)
       (%tag ,end))))

(defcm and x
  (mklogical '%go-nil))

(defcm or x
  (mklogical '%go-nnil))

(fn mkif (end x)
  (? (cdr x)
     (let next ""
       $(,(car x)
         (%go-nil ,next)
         ,(cadr x)
         (%go ,end)
         (%tag ,next)))
     (list (car x))))

(defcm ? x
  (let end ""
    $(%block
       ,@(mapcan $((x)
                    (mkif ,end x))
                 (group x 2))
       (%tag ,end))))

(var *current-return* nil)
(var *return-tag* nil)

(fn %return (v . n)
  (? (eq *current-return* (car n))
     $(%block
        ,v
        (%go ,*return-tag*))
     $(return ,v ,@n)))

(fn returnexpand (x)
  (with-global *macros* (list (cons 'return %return))
    (macroexpand x)))

(fn %block (n . body)
  (let end ""
    (!= (@ macroexpand body)
      (= *current-return* n)
      (= *return-tag* end)
      $(%block
         ,@(@ returnexpand !)
         (%tag ,end)))))

(fn blockexpand (x)
  (with-global *macros* (list (cons 'block %block))
    (macroexpand x)))

(fn cmacroexpand (x)
  (blockexpand
    (with-global *macros* *cmacros*
      (macroexpand x))))
