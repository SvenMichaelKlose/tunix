(fn unique (x)
  ;"Unique elements of a list."
  (and x
       (? (member (car x) (cdr x))
          (unique (cdr x))
          (cons (car x) (unique (cdr x))))))

(message "Testing UNIQUE...")
(or (equal (unique '(l l i i s s p p))
           '(l i s p))
    (error))
