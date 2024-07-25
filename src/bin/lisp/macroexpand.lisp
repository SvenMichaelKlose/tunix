(var *macros* nil)

(fn %requote (x)
  ;"Put expression into an unquote."
  (cons (list (caar x) (macroexpand (cadar x)))
        (%unquote (cdr x))))

(fn %unquote (x)
  ;"Expand unquotes in quasiquoted X."
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

(special macro (name . lfun)
  ;"Define macro function."
  (? (member name *macros*)
     (error name))
  (= *macros* (cons name *macros*))
  (eval (macroexpand $(fn ,name
                        ,@lfun))))

(fn macro? (s)
  ;"Test if symbol is a macro."
  (? (symbol? s)
     (member s *macros*)))

(fn macroexpand (x)
  ;"Expand *MACROS* in X."
  (?
    (or (atom x)
        (eq (car x) 'quote))
      x
    (eq (car x) 'quasiquote)
      (%unquote x)
    (macro? (car x))
      (macroexpand (apply (value (car x)) (cdr x)))
    (@ macroexpand x)))

(out "Testing macro-expansion of dotted pair...")(terpri)
(or (equal (macroexpand '(v . body))
           '(v . body))
    (error (macroexpand '(v . body))))

(out "Testing macro-expansion of unquoted list...")(terpri)

(out "(Part 1...)")(terpri)
(or (equal (macroexpand ',(a))
           ',(a))
    (error (macroexpand ',(a))))

(out "(Part 2...)")(terpri)
(or (equal (macroexpand '(,(a)))
           '(,(a)))
    (error (macroexpand '(,(a)))))

(out "(Part 3...)")(terpri)
(or (equal (macroexpand '((,(a))))
           '((,(a))))
    (error (macroexpand '((,(a))))))

(out "(Part 4...)")(terpri)
(or (equal (list (macroexpand '((,(a)))))
           '(((,(a)))))
    (error (list (macroexpand '((,(a)))))))

(out "(Part 5...)")(terpri)
(or (equal (list 2 (macroexpand '((,(a)))))
           '(2 ((,(a)))))
    (error (list 2 (macroexpand '((,(a)))))))

(out "Testing %REQUOTE...")(terpri)
(or (equal (%requote '(,a))
           '(,a))
    (error (%requote '(,a))))
(or (equal (%requote '(,(a)))
           '(,(a)))
    (error (%requote '(,(a)))))

(out "Testing MACROEXPAND...")(terpri)
(or (equal (macroexpand '$(a ,(b) ,@(c)))
           '$(a ,(b) ,@(c)))
    (error "%requote went wrong"))
