(require '!= '+@)
(in-package 'c/bf '(%block? fold))

(fn %block? (x)
  (and (cons? x)
       (eq '%block x.)))

(fn fold (x)
  (? (%block? x)
     (!= .x
       (? (cons? !)
          (+@ fold !)
          (.. !)))
     (.. x)))

(var compiler/fold-block fold)

(in-package nil)
