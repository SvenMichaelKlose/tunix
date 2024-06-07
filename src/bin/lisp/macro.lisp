; Macros are regular functions that need to be listed here.
(var *macros* nil)

(fn macro? (s)
  (and (symbol? s)
       (find s *macros*)))

(fn macroexpand-unquote (x)
  x)

(fn macroexpand (x)
  (? (cons? x)
     (?
       (eq (car x) 'quote)
         x
       (eq (car x) 'quasiquote)
         (macroexpand-unquote x)
       (@ macroexpand 
          (? (macro? (car x))
             (macroexpand (apply (car x) (cdr x)))
             x)))
     x))

;(print (macroexpand '(1 2 3 4)))

(fn let (n v . body)
  $(((,n)
      ,@body
    ,v)))

(fn with (inits . body)
  $(((,(carlist inits))
      ,@body)
    ,@(cdrlist inits)))

(fn progn body
  $(block t ,@body))

(fn prog1 body
  (let g (symbol)
    $(((,g)
        ,@(cdr body)
        ,g)
      ,(car body))))

(fn when (x . body)
  $(? ,x (progn ,@body)))

(fn unless (x . body)
  $(? ,x nil (progn ,@body)))

(fn while (cond result . body)
  (let tag (symbol)
    $(block nil
       ,tag
       (or ,cond (return))
       ,@body
       (go ,tag))))

(fn dolist (iter . body)
  (with (v    (car iter)
         i    (cadr iter)
         r    (cddr iter)
         tag  (symbol))
    $(let ,v ,i
       (block nil
         (or ,v (return ,(car r)))
         ,tag
         ,body
         (= ,v (cdr ,v))
         (go ,tag)))))

(fn push (v l)
  (= ,l (cons ,v ,l)))

(fn pop (l)
  (prog1 (car l)
    (= ,l (cdr ,l))))
