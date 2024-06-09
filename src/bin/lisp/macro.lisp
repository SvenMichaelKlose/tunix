(var *macros* '(macro progn))

; Define macro function.
(fn macro (name . lfun)
  (? (member name *macros*)
     (error "Macro already there."))
  (= *macros* (cons name *macros*))
  $(fn ,name ,@lfun))

(fn macro? (s)
  (and (symbol? s)
       (print (member s *macros*))))

(fn %requote (x which)
  (cons (cons which (macroexpand (cdr (car x))))
        (%unquote (cdr x))))

(fn %unquote (x)
  (?
    (atom x)
      x
    (atom (car x))
      (cons (car x) (%unquote (cdr x)))
    (eq (caar x) 'unquote)
      (%requote x 'unquote)
    (eq (caar x) 'unquote-splice)
      (%requote x 'unquote-splice)
    (cons (%unquote (car x))
          (%unquote (cdr x)))))

(fn macroexpand (x)
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

; Introduce local variable.
(fn let (n v . body)
  $(((,n)
      ,@body
    ,v)))

; Introduce local variables.
(fn with (inits . body)
  $(((,(carlist inits))
      ,@body)
    ,@(cdrlist inits)))

; Return value of first expression.
(fn prog1 body
  (let g (symbol)
    $(((,g)
        ,@(cdr body)
        ,g)
      ,(car body))))

; Return value of last expression.
(fn progn body
  $(block t ,@body))

(print (macroexpand '(progn some atoms)))

; Evaluate block when...
(fn when (x . body)
  $(? ,x (progn ,@body)))

; Evaluate block unless...
(fn unless (x . body)
  $(? ,x nil (progn ,@body)))

; Evaluate BODY while COND is true and return RESULT.
(fn while (cond result . body)
  (let tag (symbol)
    $(block nil
       ,tag
       (or ,cond (return))
       ,@body
       (go ,tag))))

; Iterate over conses of a list.
(fn dolist (iter . body)
  (with (v       (car iter)
         init    (cadr iter)
         result  (cddr iter)
         tag     (symbol))
    $(let ,v ,init
       (block nil
         (or ,v (return ,(car result)))
         ,tag
         ,body
         (= ,v (cdr ,v))
         (go ,tag)))))

; Destructively push onto stack.
(fn push (v l)
  $(= ,l (cons ,v ,l)))

; Destructively pop from stack.
(fn pop (l)
  $(prog1 (car l)
     (= ,l (cdr ,l))))
