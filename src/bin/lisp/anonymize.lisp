; ⚠️  UNDER CONSTRUCTION ⚠️ 
;
; Replace argument symbols by a unnamed ones
; from a static list to make space.

(fn anonymize-backquote (x syms nsyms)
  (?
    (atom x)
      x
    (eq 'unquote (car x))
      (list 'unquote
            (anonymize-expr (cadr x) syms nsyms))
    (@ $((x)
          (anonymize-expr x ,syms ,nsyms))
       x)))

(fn anonymize-expr (x syms nsyms)
  (?
    (symbol? x)
      (or (cdr (assoc x syms)) x)
    (atom x)
      x
    (eq 'fn (car x))
      (cons 'fn
            (anonymize-funexpr (cdr x) syms nsyms))
    (eq 'backquote (car x))
      (list 'backquote
            (anonymize-backquote (cadr x) syms nsyms))
    x))

(fn anonymize-funexpr (f syms nsyms)
  (let nsyms
         (@ '((x)
               (cons x (symbol)))
            (undotted (car x)))
    (@ $((x)
          (anonymize-expr x ,syms ,nsyms))
       (cdr f))))

(fn anonymize (x)
  (anonymize0 x nil nil))

(= anonymize (anonymize anonymize))
