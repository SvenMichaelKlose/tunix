; Introduce local variable.
(macro let (n v . body)
  $(((,n)
      ,@body
    ,v)))

; Introduce local variables.
(macro with (inits . body)
  $(((,(carlist inits))
      ,@body)
    ,@(cdrlist inits)))

; Return value of first expression.
(macro prog1 body
  (let g (symbol)
    $(((,g)
        ,@(cdr body)
        ,g)
      ,(car body))))

; Return value of last expression.
(macro progn body
  $(block t ,@body))

; Evaluate block when...
(macro when (x . body)
  $(? ,x (progn ,@body)))

; Evaluate block unless...
(macro unless (x . body)
  $(? ,x nil (progn ,@body)))

; Evaluate BODY while COND is true and return RESULT.
(macro while (cond result . body)
  (let tag (string)
    $(block nil
       ,tag
       (or ,cond (return))
       ,@body
       (go ,tag))))

; Iterate over conses of a list.
(macro dolist (iter . body)
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
(macro push (v l)
  $(= ,l (cons ,v ,l)))

; Destructively pop from stack.
(macro pop (l)
  $(prog1 ,(car l)
     (= ,l (cdr ,l))))
