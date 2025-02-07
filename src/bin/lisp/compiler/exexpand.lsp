(require '!= 'mapcar '+@)
(in-package 'c/ee '(move-args expand))

(def-filter move-args x
  (!= (symbol)
    (. ! $((%= ,! ,x)))))

; Move arguments out of function call
; and assign them to temporaries.
(fn expand (x)
  ; @ move-arg returns:
  ; carlist: New arguments.
  ; cdrlist: Off-loaded expressions,
  ;          assigning to temporaries.
  (?
    (atom x)
      (.. x)
    (eq '%= x.)
      (!= (move-args (cdr ..x.))
        (+ (+@ expand (+@ cdr !))
           $((%= ,.x.
                 (,(car ..x.)
                     ,@(@ car !))))))
    (!= (move-args .x)
      (+ (+@ expand (+@ cdr !))
         $((,x. (@ car !)))))))

(var compiler/exexpand expand)

(in-package nil)
