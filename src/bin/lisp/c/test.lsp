(= *alv?* t) ; Verbose AUTOLOAD.

(message "Testing CMACROEXPAND...")
(reset!)
(load 'c/cmacroexpand.lsp)
(print (c/cmacroexpand '(and a b c)))
(print (c/cmacroexpand '(or a b c)))
(print (c/cmacroexpand '(? a b c)))

(message "Testing ARGEXPAND...")
(reset!)
(and (argexpand nil nil)
     (error))
(or (equal (argexpand '(a b c)
                      '(1 2 3))
           '((a 1) (b 2) (c 3)))
    (error))
(or (equal (argexpand '(a b . c)
                      '(1 2 3))
           '((a 1) (b 2) (c (3))))
    (error))
(or (equal (argexpand '(a b . c)
                      '(1 2 3 4))
           '((a 1) (b 2) (c (3 4))))
    (error))

(message "Testing FOLD-BLOCK...")
(reset!)
(or (cequal (c/fold-block nil)
            '(nil))
    (error))
(or (cequal (c/fold-block '(%block a b))
            '(a b))
    (error))
(or (cequal (c/fold-block '(%block
                           a
                           (%block
                             b
                             c)))
            '(a b c))
    (error))

(message "Testing EXEXPAND...")
(reset!)
(print (c/exexpand (print '(a b c d))))
(print (c/exexpand (print '(a (b c d)))))
(print (c/exexpand (print '(a (b (c d))))))
