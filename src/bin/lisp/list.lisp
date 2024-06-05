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

(fn member (n x)
  (? (list? x)
     (? x
        (or (? (eq n (car x))
               x)
            (member n (cdr x))))
     (error)))

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
