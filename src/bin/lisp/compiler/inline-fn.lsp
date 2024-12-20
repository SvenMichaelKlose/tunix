(load 'compiler/package.lsp)
(require 'argexpand 'let 'do 'with-queue)

(fn inline-fn (x)
  (?
    (and (cons? x)
         (cons? x.))
      (!= (argexpand x.. .x)
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

(in-package nil)
