; See doc/compiler.md

(fn inline-fn (x)
  (?
    (and (cons? x)
         (cons? (car x)))
      (!= (argexpand (caar x) (cdr x))
        (let (argsyms (@ car !))
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
