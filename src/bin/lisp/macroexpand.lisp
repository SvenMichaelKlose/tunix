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

(message "Testing macro-expansion of dotted pair...")
(or (equal (macroexpand '(v . body))
           '(v . body))
    (error (macroexpand '(v . body))))

(message "Testing macro-expansion of unquoted list...")

(message "(Part 1...)")
(or (equal (macroexpand ',(a))
           ',(a))
    (error (macroexpand ',(a))))

(message "(Part 2...)")
(or (equal (macroexpand '(,(a)))
           '(,(a)))
    (error (macroexpand '(,(a)))))

(message "(Part 3...)")
(or (equal (macroexpand '((,(a))))
           '((,(a))))
    (error (macroexpand '((,(a))))))

(message "(Part 4...)")
(or (equal (list (macroexpand '((,(a)))))
           '(((,(a)))))
    (error (list (macroexpand '((,(a)))))))

(message "(Part 5...)")
(or (equal (list 2 (macroexpand '((,(a)))))
           '(2 ((,(a)))))
    (error (list 2 (macroexpand '((,(a)))))))

(message "Testing %REQUOTE...")
(or (equal (%requote '(,a))
           '(,a))
    (error (%requote '(,a))))
(or (equal (%requote '(,(a)))
           '(,(a)))
    (error (%requote '(,(a)))))

(message "Testing MACROEXPAND...")
(or (equal (macroexpand '$(a ,(b) ,@(c)))
           '$(a ,(b) ,@(c)))
    (error "%requote went wrong"))
