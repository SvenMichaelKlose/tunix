(fn inline-fn (x)
  (?
    (and (cons? x)
         (cons? (car x)))
      (!= (argexpand (caar x) (cdr x))
        (let argsyms (carlist !)
          $(%block
             ,@(!? argsyms
                   $((%push ,@!)))
             ,@(@ '((x) $(= ,@x)) !)
             ,@(inline-fns (cdar x))
             ,@(!? argsyms
                   $((%pop ,@!))))))
    (cons? x)
      (inline-fns x)
    x))

(fn inline-fns (x)
  (@ inline-fn x))

(print (inline-fn '((nil (print "6502 inside")))))
(print (inline-fn '((x (print "6502 inside")) 1 2)))
(print (inline-fn '(((a b) (print "6502 inside")) 1 2)))
