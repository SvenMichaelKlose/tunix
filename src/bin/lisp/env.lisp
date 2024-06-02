(fn cadr (x)
  (car (cdr x)))

(fn cddr (x)
  (cdr (cdr x)))

(fn length (x)
  (? (cons? x)
     (+ 1 (length (cdr x)))
     0))

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
       (macro? (car x))
         (apply (car x) (cdr x))
       (cons (car x)
             (@ macroexpand (cdr x))))
     x))

(print (macroexpand '(foo bar)))

(fn make-count (n)
  (? (not (== 0 n))
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

(block-test 10000)
