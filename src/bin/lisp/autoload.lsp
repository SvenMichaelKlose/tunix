(load "when.lsp")
(load "progn.lsp")
(load "group2.lsp")
(load "with.lsp")

; Be verbose during AUTOLOAD.
(var *alv?* t)

; Name to filename translations.
(var *alx*
  '((!? . aif)
    (with* . with2)))

; Try to load file for missing procedure.
; Returns non-NIL if file load was successful.
; Doesn't necessarily mean that the desired definition
; came with it.
(fn %aload (%n)
  (with (%f (symbol (nconc (symbol-name (or (cdr (assoc %n *alx*)) %n))
                           (symbol-name ".lsp"))))
    (with (%! (open %f 'r))
      (when %!
        (close %!)
        (load %f)))))

(fn %al (%code %top %x %i)
  (block nil
    (?
      ; Fix and return missing function type argument.
      (== 1 %code) ; ERROR_TYPE
        (and (eq %i 'function)
             (symbol? %x)
             (%aload %x)
             (return (symbol-value %x)))
      ; Fix and restart function call.
      (== 5 %code) ; ERROR_NOT_FUNCTION
        (when (%aload (car %x))
          ; If missing function turns out to be
          ; a macro, replace it by its expansion.
          (when (macro? (car %x))
            (with (%m (macroexpand %x))
              (setcar %x (car %m))
              (setcdr %x (cdr %m))))
          ; Try again.
          (return (eval %x))))
    ; Couldn't fix the problem.
    ; Pass it on to the debugger.
    '%fail))

(fn autoload (%code %top %x %i)
  (with (%v? *v?*)
    (= *v?* *alv?*)
    (with (%! (%al %code %top %x %i))
      (= *v?* %v?)
      %!)))

(= onerror autoload)
