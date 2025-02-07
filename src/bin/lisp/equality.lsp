(fn eql (a b)
  (? (and (number? a)
          (number? b))
     (== a b)
     (eq a b)))

(fn equal (a b)
  ;"Compare tree with EQL."
  (or (and (cons? a)
           (cons? b)
           (equal (car a) (car b))
           (equal (cdr a) (cdr b)))
      (eql a b)))
