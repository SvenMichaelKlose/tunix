; Return argument in a list.
(fn list x
  x)

; Test if list or end of list.
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

; Copy tree.
(fn copy (x)
  (? (cons? x)
     (cons (car x) (cdr x))
     x))

; Compare tree.
(fn equal (a b)
  (? (and (cons? a)
          (cons? b))
     (equal (car x) (cdr x))
     (eql a b)))

; Get first elements of lists.
(fn carlist (x)
  (@ car x))

; Get rest elements of lists.
(fn cdrlist (x)
  (@ cdr x))

; Find element in list.
(fn find (x l)
  (? (list? l)
     (? x
        (? (eq x (car l))
           x
           (find x (cdr l))))
     (error)))

; Find in associative list (CAR is key, CDR is value).
(fn assoc (v x)
  (? (cons? x)
     (? (cons? (car x))
        (? (eq v (caar x))
           x
           (assoc v (cdr x)))
        (error))))
