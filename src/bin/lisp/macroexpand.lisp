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

(fn macro? (x)
  (? (symbol? x)
     (assoc x *macros*)))

(special macro (n a . body)
  (? (macro? n)
     (setcdr (macro? n) (cons a body))
     (= *macros* (cons (cons n (cons a body))
                       *macros*)))
  (print $(macro ,n ,a))(terpri)
  (eval $(= ,n '(,a ,@(@ macroexpand body)))))

(fn macroexpand (x)
  (?
    (or (atom x)
        (eq (car x) 'quote))
      x
    (eq (car x) 'quasiquote)
      (%unquote x)
    (macro? (car x))
      (macroexpand (apply (cdr (macro? (car x))) (cdr x)))
    (@ macroexpand x)))
