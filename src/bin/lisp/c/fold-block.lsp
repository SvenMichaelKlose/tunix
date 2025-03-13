(in-package 'c/bf '(%block?))
(require '!= '+@)

(fn %block? (x)
  (and (cons? x)
       (eq '%block x.)))

(fn c/fold-block (x)
  (? (%block? x)
     (!= .x
       (? (cons? !)
          (+@ c/fold-block !)
          (.. !)))
     (.. x)))

(in-package nil)
