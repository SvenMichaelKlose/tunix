; ⚠️  UNDER CONSTRUCTION ⚠️
;
; Replace argument symbols by unnamed
; ones from static list.

(var *gsyms*
     (with-queue q
       (dotimes (i 32 (queue-list q))
         (enqueue q (symbol)))))

(fn anonymize-backquote (x syms gsyms)
  (?
    (atom x) x
    (eq 'unquote x.)
      (.. 'unquote
          (anonymize-expr .x. syms gsyms))
    (anonymize-list x syms gsyms)))

(fn anonymize-expr (x syms gsyms)
  (?
    (symbol? x)
      (or (cdr (assoc x syms)) x)
    (atom x)
      x
    (eq x. 'quote)
      x
    (eq x. 'fn)
      (. 'fn (anonymize-funexpr .x syms gsyms))
    (eq x. 'backquote)
      (.. 'backquote (anonymize-backquote .x. syms gsyms))
    (eq x. 'block)
      (. 'block (. .x. (anonymize-list (cddr x) syms gsyms)))
    (cons? x.)
      (. (anonymize-funexpr x. syms gsyms)
         (anonymize-list .x syms gsyms))
    (anonymize-list x syms gsyms)))

(fn anonymize-list (x syms gsyms)
  (@ $((x)
        (anonymize-expr x ',syms ',gsyms))
     x))

(fn xlated-args (q x gsyms)
  (?
    (not x) nil
    (atom x)
      (progn
        (enqueue q (. x gsyms.))
        gsyms.)
    (progn
      (enqueue q (. x. gsyms.))
      (. gsyms.
         (xlated-args q .x .gsyms)))))

(fn anonymize-funexpr (x syms gsyms)
  (let (q (make-queue))
    (!= (xlated-args q x. gsyms)
      (let (asyms (queue-list q))
        (. !
           (anonymize-list .x
                           (append asyms syms)
                           (nthcdr (length syms) gsyms)))))))

(fn anonymize (x)
  (anonymize-funexpr x nil *gsyms*))

(dolist (i *universe*)
  (when (symbol? i)
    (!= (symbol-value i)
      (when (cons? !.)
        (fresh-line)
        (print i)
        (fresh-line)
        (print !)
        (set i (print (anonymize !)))))))
