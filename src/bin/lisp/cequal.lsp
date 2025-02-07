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
  "EQUAL, always matching anonymous symbols."
  (or (and (cons? a)
           (cons? b)
           (cequal a. b.)
           (cequal .a .b))
      (ceql a b)))
