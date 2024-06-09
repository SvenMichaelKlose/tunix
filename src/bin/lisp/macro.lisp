; Playground for developing macro expansion functions

; Macros are regular functions that need to be here to be
; recognized my MACRO?.
(var *macros* nil)

(fn macro? (s)
  (and (symbol? s)
       (member s *macros*)))

(fn %macroexpand-unquote (x)
  ; Do it ye lazy bugger!
  x)

(fn macroexpand (x)
  (? (cons? x)
     (?
       (eq (car x) 'quote)
         x
       (eq (car x) 'quasiquote)
         (%macroexpand-unquote x)
       (@ macroexpand 
          (? (macro? (car x))
             (macroexpand (apply (car x) (cdr x)))
             x)))
     x))

;(print (macroexpand '(1 2 3 4)))

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
  (= ,l (cons ,v ,l)))

; Destructively pop from stack.
(fn pop (l)
  (prog1 (car l)
    (= ,l (cdr ,l))))
