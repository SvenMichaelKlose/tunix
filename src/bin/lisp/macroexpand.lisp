(var *macros* nil)

(special macro (name . lfun)
  ;"Define macro function."
  (? (member name *macros*)
     (error name))
  (= *macros* (cons name *macros*))
  (eval $(fn ,name
           ,@lfun)))

(fn macro? (s)
  ;"Test if symbol has a macro."
  (and (symbol? s)
       (member s *macros*)))

(fn %requote (x which)
  ;"Put expression into an unquote."
  (cons (cons which (macroexpand (cdar x)))
        (%unquote (cdr x))))

(fn %unquote (x)
  ;"Expand unquotes in quasiquoted X."
  (?
    (atom x)
      x
    (atom (car x))
      (cons (car x) (%unquote (cdr x)))
    (eq (caar x) 'unquote)
      (%requote x 'unquote)
    (eq (caar x) 'unquote-spliced)
      (%requote x 'unquote-spliced)
    (cons (%unquote (car x))
          (%unquote (cdr x)))))

(fn macroexpand (x)
  ;"Expand *MACROS* in X."
  (?
    (atom x)
      x
    (eq (car x) 'quote)
      x
    (eq (car x) 'quasiquote)
      (%unquote x)
    (macro? (car x))
      (macroexpand (apply (value (car x)) (cdr x)))
    (@ macroexpand x)))
