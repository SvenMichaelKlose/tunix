(in-package 'c/cm '(mklogical mkif))

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

(in-package nil)
