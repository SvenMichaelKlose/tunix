; ⚠️  UNDER CONSTRUCTION ⚠️
;
; Replace argument symbols by a unnamed ones
; from a static list to make space.

(var *gsyms* (with-queue q
               (dotimes (i 32 (queue-list q))
                 (enqueue q (symbol)))))

(fn anonymize-backquote (x syms gsyms)
  (?
    (atom x)
      x
    (eq 'unquote (car x))
      (list 'unquote
            (anonymize-expr (cadr x) syms gsyms))
    (anonymize-list x syms gsyms)))

(fn anonymize-expr (x syms gsyms)
  (?
    (symbol? x)
      (or (cdr (assoc x syms)) x)
    (atom x)
      x
    (eq 'quote (car x))
      x
    (eq 'fn (car x))
      (. 'fn (anonymize-funexpr (cdr x) syms gsyms))
    (eq 'backquote (car x))
      (list 'backquote (anonymize-backquote (cadr x) syms gsyms))
    (eq 'block (car x))
      (. 'block (. (cadr x) (anonymize-list (cddr x) syms gsyms)))
    (cons? (car x))
      (. (anonymize-funexpr (car x) syms gsyms)
         (anonymize-list (cdr x) syms gsyms))
    (anonymize-list x syms gsyms)))

(fn anonymize-list (x syms gsyms)
  (@ $((x)
        (anonymize-expr x ',syms ',gsyms))
     x))

(fn xlated-args (q x gsyms)
  (?
    (not x)
      nil
    (atom x)
      (progn
        (enqueue q (. x (car gsyms)))
        (car gsyms))
    (progn
      (enqueue q (. (car x) (car gsyms)))
      (. (car gsyms)
         (xlated-args q (cdr x) (cdr gsyms))))))

(fn anonymize-funexpr (x syms gsyms)
  (let (q (make-queue))
    (!= (xlated-args q (car x) gsyms)
      (let (asyms (queue-list q))
        (. ! (anonymize-list (cdr x)
                             (append asyms syms)
                             (nthcdr (length syms) gsyms)))))))

(fn anonymize (x)
  (anonymize-funexpr x nil *gsyms*))

(dolist (i *universe*)
  (when (symbol? i)
    (!= (symbol-value i)
      (when (cons? (car !))
        (fresh-line)
        (print i)
        (fresh-line)
        (print !)
        (set i (print (anonymize !)))))))
