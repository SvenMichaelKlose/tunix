(message "# Compiling environment")

(message "# Dot- and standard macro-expansion...")
(require 'let 'with-out 'prog1 'progn
         'awhen 'do 'dolist)
(with-out o (open '_1dotsmacros.lsp 'w)
  (dolist (i (member 'autoload
                     *universe*))
    (or (builtin? i)
        (awhen (symbol-value i)
          (and (cons? !)
               (list? !.)
               (not (== \* (char-at i 0)))
               (not (== \+ (char-at i 0)))
               (progn
                 (setout stdout)
                 (print i)
                 (setout o)
                 (print (. i (. !. (@ '((x) (macroexpand (dotexpand x))) .!))))))))))
(reset!)

(message "# Compiler macro expansion...")
(load 'compiler/cmacroexpand.lsp)
(load 'compiler/pass.lsp)
(compiler/pass
    '_1dotsmacros.lsp '_2cmacros.lsp
    compiler/cmacroexpand t)

(message "# Expression expansion...")
(reset!)
(load 'compiler/exexpand.lsp)
(compiler/pass
    '_2cmacros.lsp '_3expex.lsp
    compiler/exexpand t)

(message "# Folding blocks...")
(reset!)
(load 'compiler/fold-block.lsp)
(compiler/pass
    '_3expex.lsp '_4folded.lsp
    compiler/fold-block t)

(message "# COMPILER/COMPILE-ENV done.")
