(message "# Compiling environment")

;;;;;;;;;;;;;;;;;
;;; FRONT END ;;;
;;;;;;;;;;;;;;;;;

(message '"# Standard macro expansion...")
(with-out o (open '"_tmpa.lsp" 'w)
  (dolist (i (member 'autoload *universe*))
    (or (builtin? i)
        (awhen (symbol-value i)
          (and (cons? !)
               (list? (car !))
               (not (== \* (char-at i 0)))
               (not (== \+ (char-at i 0)))
               (progn
                 (setout stdout)
                 (print i)
                 (setout o)
                 (print (. i (. (car !) (@ macroexpand (cdr !)))))))))))

(message '"# Compiler macro expansion...")
(reset!)
(load 'compiler/cmacroexpand.lsp)
(load 'compiler/package.lsp)
; With MACROEXPAND hijacked, macros cannot be AUTOLOADed.
(with-in i (open '"_tmpa.lsp" 'r)
  (with-out o (open '"_tmpb.lsp" 'w)
    (while (not (eof))
      (awhen (read)
        (setout stdout)
        (print (car !))
        (setout o)
        (print (. (car !) (. (cadr !) (@ cmacroexpand (cddr !)))))
        (reset!)))))

(message '"# TODO: Collect function info...")
(message '"# TODO: Argument expansion...")

(message '"# Inlining anonymous functions...")
(reset!)
(load 'compiler/inline-fn.lsp)
(load 'compiler/package.lsp)
(with-in i (open '"_tmpb.lsp" 'r)
  (with-out o (open '"_tmpc.lsp" 'w)
    (while (not (eof))
      (awhen (read)
        (setout stdout)
        (print (car !))
        (setout o)
        (print (. (car !) (. (cadr !) (@ inline-fn (cddr !)))))
        (reset!)))))

(message '"# TODO: Expression expansion...")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FRONT END CLEANUP ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(message '"# Folding blocks...")
(reset!)
(load 'compiler/fold-block.lsp)
(load 'compiler/package.lsp)
(with-in i (open '"_tmpc.lsp" 'r)
  (with-out o (open '"_tmpd.lsp" 'w)
    (while (not (eof))
      (awhen (read)
        (setout stdout)
        (print (car !))
        (setout o)
        (print (. (car !) (. (cadr !) (mapcan fold-block (cddr !)))))
        (reset!)))))

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
