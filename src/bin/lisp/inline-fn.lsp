(app 'app-inline-fn)

(require 'reverse)

(fn inline-fn (x)
  (?
    (and (cons? x)
         (cons? (car x)))
      (!= (argexpand (caar x) (cdr x))
        (let (argsyms (carlist !))
          $(%block
             ,@(!? argsyms
                   $((%push ,@!)))
             ,@(@ '((x) $(= ,@x)) !)
             ,@(@ inline-fn (cdar x))
             ,@(!? argsyms
                   $((%pop ,@(reverse !)))))))
    (cons? x)
      (@ inline-fn x)
    x))

(app 'app-inline-fn)
