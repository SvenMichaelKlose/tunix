(fn list x
  x)

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

(fn copy (x)
  (? (cons? x)
     (cons (car x) (cdr x))
     x))

(fn equal (a b)
  (? (and (cons? a)
          (cons? b))
     (equal (car x) (cdr x))
     (eql a b)))

(fn carlist (x)
  (@ car x))

(fn cdrlist (x)
  (@ cdr x))

(fn find (x l)
  (? (list? l)
     (? x
        (? (eq x (car l))
           x
           (find x (cdr l))))
     (error)))

(fn assoc (v x)
  (? (cons? x)
     (? (cons? (car x))
        (? (eq v (caar x))
           x
           (assoc v (cdr x)))
        (error))))
