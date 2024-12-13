(message "# Compiling environment")

;;;;;;;;;;;;;;;;;
;;; FRONT END ;;;
;;;;;;;;;;;;;;;;;

(message '"# Dot- and standard macro-expansion...")
; To really expand all macros there has to be a list of macros
; available in the environment.
(require 'let 'with-out 'dolist 'prog1 awhen progn)
(with-out o (open '_1dotsmacros.lsp 'w)
  (dolist (i (member 'autoload *universe*))
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

(message '"# Compiler macro expansion...")
(reset!)
(load 'compiler/cmacroexpand.lsp)
(load 'compiler/pass.lsp)
(compiler/pass '_1dotsmacros.lsp '_2cmacros.lsp compiler/cmacroexpand)

;(message '"# TODO: Collect function info...")
;(message '"# TODO: Argument expansion...")

(message '"# Inlining anonymous functions...")
(reset!)
(load 'compiler/inline-fn.lsp)
(compiler/pass '_2cmacros.lsp '_3inlined.lsp compiler/inline-fn)

;(message '"# TODO: Expression expansion...")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FRONT END CLEANUP ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(message '"# Folding blocks...")
(reset!)
(load 'compiler/fold-block.lsp)
(compiler/pass '_3inlined.lsp '_4folded.lsp compiler/fold-block)

;(message '"# Straighten jumps...")
;(message '"# Remove unused tags...")

;;;;;;;;;;;;;;;;;;;;
;;; OPTIMIZATION ;;;
;;;;;;;;;;;;;;;;;;;;

;(message '"# TODO: Fold constants...")
;(message '"# TODO: Remove temporaries...")
;(message '"# TODO: Remove useless stores...")
;(message '"# TODO: Remove useless loads...")
;(message '"# TODO: Remove unused code...")

;;;;;;;;;;;;;;;;;
;;; BACK END ;;;;
;;;;;;;;;;;;;;;;;

;(message '"# TODO: Code macros...")
;(message '"# TODO: Remove temporaries...")
;(message '"# TODO: Remove useless stores...")
;(message '"# TODO: Remove useless loads...")
;(message '"# TODO: Remove unused code...")

;(fn compile-env () (reset!))
