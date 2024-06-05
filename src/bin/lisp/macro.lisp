(out "Loading environment.")(terpri)
(out "Please wait...")(terpri)

(fn list x
  x)

(fn caar (x)
  (car (car x)))

(fn cadr (x)
  (car (cdr x)))

(fn cdar (x)
  (cdr (car x)))

(fn cddr (x)
  (cdr (cdr x)))

(fn member (n x)
  (? x
     (or (and (eq n (car x)) x)
         (member n (cdr x)))))

(var *macros* nil)

(fn find (x l)
  (and (cons? l)
       (? (eq x (car l))
          x
          (find x (cdr l)))))

(fn macro? (s)
  (find s *macros*))

(fn macroexpand-bq (x)
  x)

(fn macroexpand (x)
  (? (cons? x)
     (?
       (eq (car x) 'quote)
         x
       (eq (car x) 'backquote)
         (macroexpand-bq x)
       (@ macroexpand 
          (? (macro? (car x))
             (macroexpand (apply (car x) (cdr x)))
             x)))
     x))

(fn assoc (v x)
  (? (cons? x)
     (? (cons? (car x))
        (? (eq v (caar x))
           (cdar x)
           (assoc v (cdr x)))
        (error))))

(fn make-count (n)
  (? (not (< 1 n))
     (cons n (make-count (-- n)))))

(fn block-test (c)
  (out "Looping ")
  (print c)
  (out " times...")
  (terpri)
  (block nil
    tag
    (= c (-- c))
    (? (== 0 (% c 100))
       (print c))
    (? (not (== c 0))
       (go tag))))

(fn progn body
  ^(block t
     ,@body))

(fn let (n v . body)
  (((g)
     ^(((,n)
         ,@body
       ,v)))
   (symbol)))

(fn prog1 body
  (with (g (symbol))
    ^(((,g)
        ,@(cdr body)
        ,g)
      (car body))))

(fn when (x . body)
  ^(? ,x
      (progn
        ,@body)))

(fn unless (x . body)
  ^(? (not ,x)
      (progn
        ,@body)))

(fn while (cond result . body)
  (let tag (symbol)
    ^(block nil
       (or ,cond
           (return))
       ,@body)))

(fn dolist (iter . body)
  (with (v   (car iter)
         i   (cadr iter)
         r   (cddr iter)
         tag (symbol))
    ^(let ,v ,i
       (block nil
         (or ,v (return ,(car r)))
         ,tag
         ,body
         (= ,v (cdr v))
         (go ,tag)))))

(print (gc))(out " bytes free.")(terpri)
