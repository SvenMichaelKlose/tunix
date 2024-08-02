(= x 42)

(message "Testing EQUAL...")
(or (equal '(1 2) '(1 2))
    (error))

(message "Testing APPLY...")
(or (equal (apply '(x x) '(1))
           '(1))
    (error))
(or (equal (apply '(x x) '(1 2))
           '(1 2))
    (error))
(or (equal (apply '(x x) 1 2 '(3 4))
           '(1 2 3 4))
    (error))

(message "Testing EVAL...")
(or (equal (eval '(list 1 2 3))
           '(1 2 3))
    (error (eval '(list 1 2 3))))
(or (equal (eval '(list 1 2 ((nil 3))))
           '(1 2 3))
    (error (eval '(list 1 2 ((nil 3))))))
(or (equal '(1 . 2)
           (cons 1 2))
    (error "Dotted pair not read OK"))

(message "Testing BUTLAST...")
(and (butlast nil)
     (error))
(and (butlast '(a))
     (error))
(or (equal (butlast '(a b c))
           '(a b))
    (error))

(message "Testing LAST...")
(and (last nil)
     (error))
(or (equal (last '(a))
           '(a))
    (error))
(or (equal (last '(a b))
           '(b))
    (error))

(message "Testing MEMBER...")
(and (member 'b nil)
     (error))
(or (equal (member 'b '(a b c))
           '(b c))
    (error))

(message "Testing @...")
(and (@ '((x)) nil)
     (error))
(or (equal (@ '((x)) '(a b c))
           '(a b c))
    (error))

(or (== x 42)
    (error "X was modified globally"))
