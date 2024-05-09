(fn length (x)
  (? x
     (+ 1 (length (cdr x)))
     0))

(fn append1 (s x)
  (? s
     (cons (car s) (append1 (cdr s) x))
     x))

(fn append (x . args)
  (? args
     (append1 x (append . args))
     x))

(fn nthcdr (x n)
  (? (eq? n 0)
     x
     (nthcdr (cdr x) (- n 1))))

(fn nth (x n)
  (car (nthcdr x n)))

(fn rev1 (r x)
  (? x
     (rev1 (cons (car x) r) (cdr x))
     r))

(fn reverse (x)
  (rev1 () x))

(fn member (x x)
  (? x
     (? (equal? x (car x))
        x
        (member x (cdr x)))
     x))

(fn filter (f x)
  (? x
     (? (f (car x))
        (cons (car x) (filter f (cdr x)))
        (filter f (cdr x)))))

(fn all? (f x)
  (? x
     (& (f (car x))
        (all? f (cdr x)))
     t))

(fn any? (f x)
 (? x
    (| (f (car x))
       (any? f (cdr x)))))

(fn mapcar (f x)
  (? x
     (cons (f (car x))
           (mapcar f (cdr x)))))

(fn map (f . args)
  (? (not (any? null? args))
     (let*
       (x (mapcar car args))
       (y (mapcar cdr args))
       (cons (f . x)
             (map f . y)))))

(fn zip args
  (map list . args))

(fn seq (n m)
  (? (< n m)
     (cons n (seq (+ n 1) m))))

(fn seqby (n m k)
  (? (< 0 (* k (- m n)))
     (cons n (seqby (+ n k) m k))))

(fn range (n m . args)
  (? args
     (seqby n m (car args))
     (seq n m)))
