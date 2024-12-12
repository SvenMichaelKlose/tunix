(load 'when.lsp)
(load 'group2.lsp)
(load 'let.lsp)

; Be verbose during AUTOLOAD.
(var *alv?* t)

; Name to filename translations.
(var *alx*
  '((!?   . aif)
    (do*  . do2)
    (let* . let2)))

; Try to load file for missing procedure.
; Returns non-NIL if file load was successful.
; Doesn't necessarily mean that the desired definition
; came with it.
(fn %aload (%n)
  (let (%f (symbol (nconc (symbol-name (or (cdr (assoc %n *alx*)) %n))
                          (symbol-name '".lsp"))))
    (let (%! (open %f 'r))
      (when %!
        (close %!)
        (load %f)))))

(fn %al (%code %top %x %f %i)
  (block nil
    (?
      ; Fix and return missing function type argument.
      (== 1 %code) ; ERROR_TYPE
        (and (eq %i 'function)
             (symbol? %f)
             (%aload %f)
             (return (symbol-value %f)))
      ; Fix and restart function call.
      (== 5 %code) ; ERROR_NOT_FUNCTION
        (when (or (macro? %f)
                  (%aload %f))
          ; If missing function turns out to be
          ; a macro, replace expression by its expansion.
          (when (macro? %f)
            (let (%m (*ex* %x))
              (setcar %x (car %m))
              (setcdr %x (cdr %m))))
          ; Try again.
          (return (eval %x))))
    ; Couldn't fix the problem.
    ; Pass it on to the debugger.
    '%fail))

(fn autoload (%code %top %x %f %i)
  ; Save verbosity level.
  (let (%v? *v?*)
    ; Set desired one during AUTOLOAD.
    (= *v?* *alv?*)
    ; And action!
    (let (%! (%al %code %top %x %f %i))
      ; Restore verbosity.
      (= *v?* %v?)
      %!)))

(= onerror autoload)
