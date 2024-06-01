(@ print '(1 2 3))

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

(fn macroexpand x
  (when x
    (| (atom x)
       (!? (macro? (car x))
           (funcall ! (cdr x))
           (@ macroexpand x)))))

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
