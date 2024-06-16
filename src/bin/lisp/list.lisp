(fn list x
  x)

(out "Testing LIST:")(terpri)
(print (list))(terpri)
(print (list 1 2 3))(terpri)

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

(fn copy (x)
  "Copy tree."
  (? (cons? x)
     (cons (car x) (cdr x))
     x))

(fn append (first . rest)
  "Copy and concatenate."
  (?
    first
      (cons (car first)
            (apply append (cdr first) rest))
    rest
      (apply append rest)))

(out "Testing APPEND:")(terpri)
(print (append nil nil))(terpri)
(print (append nil '(3 4)))(terpri)
(print (append '(1 2) nil))(terpri)
(print (append '(1 2) '(3 4)))(terpri)

(fn carlist (x)
  (@ car x))

(fn cdrlist (x)
  (@ cdr x))

(fn find (x l)
  (car (member x l)))
