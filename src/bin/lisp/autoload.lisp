; Load missing function.
;
; Variable names have been prefixed with
; a '~' to avoid ; clashes with the
; interrupted procedure during LOAD or
; EVAL.
(fn autoload (~code ~top ~x)
  (block nil
    ; Handle only if ERROR_NOT_FUNCTION.
    (and (== ~code 5)
         (symbol? (car ~x))
      ; Turn function name into filename
      ; by appending ".lisp" suffix.
      (((~fname)
        ; Check if file exists by opening it.
        (((~!)
           (? ~!
              ((()
                 ; It does.  Close it.
                 (close ~!)
                 ; Load it the regular way.
                 (load ~fname)
                 ; If a macro was missing, replace
                 ; expression by an expanded one.
                 (? (macro? (car ~x))
                    (((~m)
                       (setcar ~x (car ~m))
                       (setcdr ~x (cdr ~m)))
                     (macroexpand ~x)) nil)
                 ; Evaluate the failed expression again.
                 (return (eval ~x) nil)))))
          (open ~fname 'r)))
        (symbol (nconc (symbol-name (car ~x))
                        (symbol-name ".lisp")))))
    ; Tell to fail on the error as usual.
    '%fail))

(= onerror autoload)
