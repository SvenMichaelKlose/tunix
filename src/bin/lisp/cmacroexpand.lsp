(var *cmacros* nil)

(macro defcm (n a . body)
  (print $(defcm ,n ,a))
  (push (. n (. a (@ macroexpand body)))
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
  (? (cdr x)
     (let (next "")
       $(,(car x)
         (%go-nil ,next)
         ,(cadr x)
         (%go ,end)
         (%tag ,next)))
     (list (car x))))

(defcm ? x
  (let (end "")
    $(%block
       ,@(mapcan $((x)
                    (mkif ,end x))
                 (group x 2))
       (%tag ,end))))

(var *blocks* nil)

; TODO: Collect potential tags.
(fn %block (n . body)
  (let (end "")
    (push (. n end) *blocks*)
    (!= (@ macroexpand body)
      (pop *blocks*)
      $(%block
         ,@!
         (%tag ,end)))))

(fn %return (v . n)
  (!? (cdr (assoc n *blocks*))
      $(%block
         ,v
         (%go ,!))
      (error "Unknown BLOCK " n)))

(defcm block (n . body)
  (with-global *macros* (list (. 'block %block)
                              (. 'return %return))
    (apply %block n body)))

(fn cmacroexpand (x)
  (with-global *macros* *cmacros*
    (macroexpand x)))
