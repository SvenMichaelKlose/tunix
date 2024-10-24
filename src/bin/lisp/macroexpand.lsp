(var *macros* nil)

(fn %requote (x)
  (. (list (caar x) (macroexpand (cadar x)))
     (%unquote (cdr x))))

(fn %unquote (x)
  (?
    (atom x)
      x
    (atom (car x))
      (. (car x) (%unquote (cdr x)))
    (eq 'unquote (caar x))
      (%requote x)
    (eq 'unquote-spliced (caar x))
      (%requote x)
    (. (%unquote (car x))
       (%unquote (cdr x)))))

(fn macro? (x)
  (? (or (symbol? x)
         (builtin? x))
     (assoc x *macros*)))

(fn macroexpand (x)
  (?
    (or (atom x)
        (eq 'quote (car x)))
      x
    (eq (car x) 'quasiquote)
      (%unquote x)
    (macro? (car x))
      (macroexpand (apply (cdr (macro? (car x))) (cdr x)))
    (@ macroexpand x)))

(special macro (n a . body)
  ; Print message if so desired.
  (and *v?*
    (((oldout)
       (setout stdout)
       (print $(macro ,n ,a))
       (setout oldout))
     fnout))
  (((!)
     ; Either add to *MACROS* or update existing entry.
     (? (macro? n)
        (setcdr (macro? n) (. a !))
        (= *macros* (. (. n (. a !))
                    *macros*))))
    (@ macroexpand body)))

(= *ex* macroexpand)
