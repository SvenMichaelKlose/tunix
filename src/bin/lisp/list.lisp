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

(fn cddr (x)
  (cdr (cdr x)))

(fn cadar (x)
  (car (cdr (car x))))

(fn caddr (x)
  (car (cdr (cdr x))))

(fn append (first . rest)
  ;"Copy and concatenate."
  (?
    first
      (cons (car first)
            (apply append (cdr first) rest))
    rest
      (apply append rest)))

(out "Testing APPEND...")(terpri)
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
