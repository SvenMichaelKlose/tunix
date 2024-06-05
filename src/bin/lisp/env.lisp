(out "Loading environment.")(terpri)
(out "Please wait...")(terpri)

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
     (or (and (eq n (car x))
              x)
         (member n (cdr x)))
     (error)))

(fn find (x l)
  (and (cons? l)
       (? (eq x (car l))
          x
          (find x (cdr l)))))

(fn assoc (v x)
  (? (cons? x)
     (? (cons? (car x))
        (? (eq v (caar x))
           x
           (assoc v (cdr x)))
        (error))))

(fn make-count (n)
  (? (< 0 n)
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

(print (gc))(out " bytes free.")(terpri)
