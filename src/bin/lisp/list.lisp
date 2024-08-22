(fn list x
  x)

(out "Testing LIST...")(terpri)
(and (list)
     (error))
(or (equal (list 1)
           '(1))
    (error))
(or (equal (list 1 2)
           '(1 2))
    (error))
(or (equal (list 1 2 3)
           '(1 2 3))
    (error))

(fn list? (x)
  (or (not x)
      (cons? x)))

(fn caar (x)
  (car (car x)))

(fn cadr (x)
  (car (cdr x)))

(fn cdar (x)
  (cdr (car x)))

(fn cddr (x)
  (cdr (cdr x)))

(fn cadar (x)
  (car (cdr (car x))))

(fn caddr (x)
  (car (cdr (cdr x))))

(fn append x
  ;"Copy and concatenate."
  (?
    (car x)
      (cons (caar x)
            (apply append (cdar x) (cdr x)))
    (cdr x)
      (apply append (cdr x))))

(out "Testing APPEND...")(terpri)
(and (append)
     (error))
(and (append nil)
     (error))
(and (append nil nil)
     (error))
(or (equal (append nil '(3 4))
           '(3 4))
    (error))
(or (equal (append '(1 2) nil)
           '(1 2))
    (error))
(or (equal (append '(1 2) '(3 4))
           '(1 2 3 4))
    (error))

(fn carlist (x)
  (@ car x))

(fn cdrlist (x)
  (@ cdr x))
