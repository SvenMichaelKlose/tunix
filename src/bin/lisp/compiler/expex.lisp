; TUNIX Lisp compiler
;
; Break up nested function calls of a single expression into
; many ones to uniform list of instructions.
;
; This expression
;
; ~~~
; (an-expression arg1 (arg2 (arg2b) (arg2c)) (arg3))
; ~~~
;
; becomes this with temporary places inserted:
;
; ~~~
; (= (%p 1) (arg2b))
; (= (%p 2) (arg2c))
; (= (%p 3) (arg2 (%p 1) (%p 2)))
; (= (%p 4) (arg3))
; (= (%p 0) (an-expression arg1 (%p 3) (%p 4)))
; ~~~
;
; The return value of an expression list is always ~0.

(var rv (symbol))

(fn exparg (x)
  (? (atom x)
     (. x (list x))
     (!= (symbol)
       (. ! (expex x !)))))

(fn expex (x v)
  ;"Make single statement assignments of expression."
  (? (atom x)
     $((= ,(or v rv) ,x))
     (!= (@ exparg .x)
       (+ (cdrlist !)
          $((= ,(or v rv) (,x. ,@(carlist !))))))))
