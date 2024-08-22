(message "Testing top-level file read...")
(var ch (open "test-read.bin" 'r))
(or ch (error "Cannot open \"test-read.bin\"."))
(setin ch)
(and (eof)
     (error "Unexpected EOF at file start"))
(or (== \a (in))
    (error "Should read 'a'"))
(and (eof)
     (error "Unexpected EOF after 'a'"))
(or (== \b (in))
    (error "Should read 'b'"))
(and (eof)
     (error "Unexpected EOF before file end"))
(print (in))
; TODO: CBMs won't give us an EOF here.
; Maybe a REPL thing.
(and (eq +t+ 'unix)
     (or (eof)
         (error "EOF expected")))
(setin stdin)
(close ch)
(= *universe* (remove 'ch *universe*))

(message "Testing SETCAR...")
(= x (cons nil nil))
(or (equal (setcar x t)
           (cons t nil))
    (error))

(message "Testing SETCDR...")
(= x (cons nil nil))
(or (equal (setcdr x t)
           (cons nil t))
    (error))

(message "Testing SYMBOL-NAME...")
(or (equal (symbol-name 'abc)
           '(97 98 99))
    (error))

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

(message "Testing COPY-LIST...")
(and (copy-list nil)
     (error))
(or (equal (copy-list '(a))
           '(a))
    (error))
(or (equal (copy-list '(a b))
           '(a b))
    (error))
(or (equal (copy-list '(a b c))
           '(a b c))
    (error))

(message "Testing BUTLAST...")
(and (butlast nil)
     (error))
(and (butlast '(a))
     (error))
(or (equal (butlast '(a b c))
           '(a b))
    (error))

(message "Testing REMOVE...")
(and (remove 'b nil)
     (error))
(or (equal (remove 'a '(a b c))
           '(b c))
    (error))
(or (equal (remove 'b '(a b c))
           '(a c))
    (error))
(or (equal (remove 'c '(a b c))
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

(message "Testing FN redefinition...")
(fn doubledef ())
(fn doubledef ())
(and (member 'doubledef
             (cdr (member 'doubledef *universe*)))
     (error "DOUBLEDEF not alone in *UNIVERSE*."))
(= *universe* (cdr (member 'doubledef *universe*)))

(or (== x 42)
    (error "X was modified globally"))
