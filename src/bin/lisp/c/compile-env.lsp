(in-package 'c '(dotmac))

(require 'let 'with-out 'prog1 'progn
         'awhen 'do 'dolist 'remove-if
         'remove-if-not 'with-queue)

(fn dotmac (x)
  (macroexpand (dotexpand x)))

(fn compile-file (x)
  (print x)
  (with-out o (open '_1dotsmacros.lsp 'w)
    (dolist (i (ensure-list x))
      (setout stdout)
      (print i)
      (setout o)
      (print $(,i ,!. ,@(dotmax .!)))))
  (reset!)
  (load 'c/cmacroexpand.lsp)
  (load 'c/pass.lsp)
  (c/pass
      '_1dotsmacros.lsp '_2cmacros.lsp
      c/cmacroexpand t)
  (reset!)
  (load 'c/exexpand.lsp)
  (c/pass
      '_2cmacros.lsp '_3expex.lsp
      c/exexpand t)
  (reset!)
  (load 'c/fold-block.lsp)
  (c/pass
      '_3expex.lsp '_4folded.lsp
      c/fold-block t))

(fn can-import? (x)
  (and (not (builtin? x))
       (not (== \* (char-at x 0)))
       (not (== \+ (char-at x 0)))
       (awhen (symbol-value x)
         (and (cons? !)
              (list? !.)))))

(fn c/compile-env ()
  (compile-file
      (remove-if-not can-import?
          (member 'autoload *universe*))))

(in-package nil)
