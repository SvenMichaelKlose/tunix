(var *macros* nil)

(fn %requote (x)
  (cons (list (caar x) (macroexpand (cadar x)))
        (%unquote (cdr x))))

(fn %unquote (x)
  (?
    (atom x)
      x
    (atom (car x))
      (cons (car x) (%unquote (cdr x)))
    (eq (caar x) 'unquote)
      (%requote x)
    (eq (caar x) 'unquote-spliced)
      (%requote x)
    (cons (%unquote (car x))
          (%unquote (cdr x)))))

(special macro (name . x)
  (or (member name *macros*)
      (= *macros* (cons name *macros*)))
  (eval (macroexpand $(fn ,name ,@x))))

(fn macro? (s)
  (? (symbol? s)
     (member s *macros*)))

(fn macroexpand (x)
  (?
    (or (atom x)
        (eq (car x) 'quote))
      x
    (eq (car x) 'quasiquote)
      (%unquote x)
    (macro? (car x))
      (macroexpand (apply (symbol-value (car x)) (cdr x)))
    (@ macroexpand x)))
