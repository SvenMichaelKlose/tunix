(message "# Compiling environment")

;;;;;;;;;;;;;;;;;
;;; FRONT END ;;;
;;;;;;;;;;;;;;;;;

(message '"# Standard macro expansion...")
(with-out o (open '"_tmpa.lsp" 'w)
  (dolist (i (remove 'quasiquote (member 'autoload *universe*)))
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

; With MACROEXPAND hijacked, macros cannot be AUTOLOADed.
(require 'let 'prog1 'push 'pop '!= 'aif 'with-global 'with-in 'with-out 'when 'awhen 'acons! 'while 'group 'mapcar 'mapcan)
(var tag nil)
(require 'cmacroexpand)

(message '"# Compiler macro expansion...")
(with-in i (open '"_tmpa.lsp" 'r)
  (with-out o (open '"_tmpb.lsp" 'w)
    (while (not (eof))
      (awhen (read)
        (setout stdout)
        (print (car !))
        (setout o)
        (print (. (car !) (. (cadr !) (@ cmacroexpand (cddr !)))))))))

(= *universe* (cdr (member 'tag *universe*)))
(var tag nil)
(load 'inline-fn.lsp)
(load 'argexpand.lsp)
(load 'with-queue.lsp)
(load 'enqueue.lsp)
(load 'reverse.lsp)
(load 'fold-block.lsp)
(= *macros* nil)

(message '"# Collect function info...")
(message '"# Argument expansion...")

(message '"# Inlining anonymous functions...")
(with-in i (open '"_tmpb.lsp" 'r)
  (with-out o (open '"_tmpc.lsp" 'w)
    (while (not (eof))
      (awhen (read)
        (setout stdout)
        (print (car !))
        (setout o)
        (print (. (car !) (. (cadr !) (@ inline-fn (cddr !)))))))))

(message '"# Expression expansion...")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FRONT END CLEANUP ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(message '"# Folding blocks...")
(with-in i (open '"_tmpc.lsp" 'r)
  (with-out o (open '"_tmpd.lsp" 'w)
    (while (not (eof))
      (awhen (read)
        (setout stdout)
        (print (car !))
        (setout o)
        (print (. (car !) (. (cadr !) (mapcan fold-block (cddr !)))))))))

(message '"# Straighten jumps...")
(message '"# Remove unused tags...")

;;;;;;;;;;;;;;;;;;;;
;;; OPTIMIZATION ;;;
;;;;;;;;;;;;;;;;;;;;

(message '"# Fold constants...")
(message '"# Remove temporaries...")
(message '"# Remove useless stores...")
(message '"# Remove useless loads...")
(message '"# Remove unused code...")

;;;;;;;;;;;;;;;;;
;;; BACK END ;;;;
;;;;;;;;;;;;;;;;;

(message '"# Code macros...")
(message '"# Remove temporaries...")
(message '"# Remove useless stores...")
(message '"# Remove useless loads...")
(message '"# Remove unused code...")

(reset!)
