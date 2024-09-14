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
             ,@(@ inline-fn (cdar x))
             ,@(!? argsyms
                   $((%pop ,@!))))))
    (cons? x)
      (@ inline-fn x)
    x))
