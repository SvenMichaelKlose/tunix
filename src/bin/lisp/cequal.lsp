(fn ceql (a b)
  (?
    (and (number? a)
         (number? b))
      (== a b)
    (and (symbol? a)
         (== 0 (length (symbol-name a)))
         (symbol? b)
         (== 0 (length (symbol-name b))))
      t
    (eq a b)))

(fn cequal (a b)
  (or (and (cons? a)
           (cons? b)
           (cequal (car a) (car b))
           (cequal (cdr a) (cdr b)))
      (ceql a b)))
