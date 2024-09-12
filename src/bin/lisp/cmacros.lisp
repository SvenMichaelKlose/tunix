(load "mapcar.lisp")

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

(= *universe* (remove 'mklogical *universe*))

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

(= *universe* (remove 'mkif *universe*))

(fn cmacroexpand (x)
  (with-global *macros* *cmacros*
    (macroexpand x)))
