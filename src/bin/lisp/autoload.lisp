(fn ~alm (~e)
  ; Turn function name into filename
  ; by appending ".lisp" suffix.
  (((~f)
    ; Check if file exists by opening it.
    (((~!)
       (? ~!
          ((()
             ; It does.  Close it.
             (close ~!)
             ; Load it the regular way.
             (load ~f)))))
     (open ~f 'r)))
   (symbol (nconc (symbol-name ~e)
                  (symbol-name ".lisp")))))

(fn ~almr (~x)
  (? (cons? ~x)
     (or (and (symbol? (car ~x))
              (eq (symbol-value (car ~x)) (car ~x))
              (~alm (car ~x)))
         (~almr (cdr ~x)))))

; JIT-load missing functions and macros
;
; Variable names have been prefixed with
; a '~' to avoid clashes with inter-
; rupted procedures during LOAD or EVAL.
(fn autoload (~code ~top ~x)
  (block nil
    ; Handle only if ERROR_NOT_FUNCTION.
    (? (== ~code 5)
       ((()
          (? (symbol? (car ~x))
             ((()
                (? (~alm (car ~x))
                   ((()
                      ; If a macro was missing, replace
                      ; expression by an expanded one.
                      (? (macro? (car ~x))
                         (((~m)
                            (setcar ~x (car ~m))
                            (setcdr ~x (cdr ~m)))
                          (macroexpand ~x)))
                      (return (eval ~x)))))
                   (? (~almr (cdr ~x))
                      ; Evaluate the failed expression again.
                      (return (eval ~x)))))))))
    ; Tell to fail on the error as usual.
    '%fail))

(= onerror autoload)
