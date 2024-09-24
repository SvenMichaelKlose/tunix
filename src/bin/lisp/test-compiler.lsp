(or (macro? '!?=)
    (load "aif.lsp"))
(or (cons? mapcar)
    (load "mapcar.lsp"))
(or (cons? app)
    (load "app.lsp"))

(app 'app-test-compiler)

(message "Testing CMACROEXPAND...")
(print (cmacroexpand '(and a b c)))
(print (cmacroexpand '(or a b c)))
(print (cmacroexpand '(? a b c)))

(message "Testing ARGEXPAND...")
(and (argexpand nil nil)
     (error))
(or (equal (argexpand '(a b c) '(1 2 3))
           '((a 1) (b 2) (c 3)))
    (error))
(or (equal (argexpand '(a b . c) '(1 2 3))
           '((a 1) (b 2) (c (3))))
    (error))
(or (equal (argexpand '(a b . c) '(1 2 3 4))
           '((a 1) (b 2) (c (3 4))))
    (error))

(message "Testing FOLD-BLOCK...")
(or (cequal (fold-block nil)
            '(nil))
    (error))
(or (cequal (fold-block '(%block a b))
            '(a b))
    (error))
(or (cequal (fold-block '(%block a (%block b c)))
            '(a b c))
    (error))

(message "Testing INLINE-FN...")
(or (cequal (inline-fn '((())))
            '(%block))
    (error))
(or (cequal (inline-fn '((() a b)))
            '(%block a b))
    (error))
(or (cequal (inline-fn '((()
                           a b
                           (((c d)
                              e f)
                            1 2))))
            '(%block
               a b
               (%block
                 (%push c d)
                 (= c 1)
                 (= d 2)
                 e f
                 (%pop d c))))
    (error))

(message "Testing EXEXPAND...")
(load "exexpand.lsp")
(print (exexpand (print '(a b c d))))
(print (exexpand (print '(a (b c d)))))
(print (exexpand (print '(a (b (c d))))))

(load "bcasm.lsp")

(app 'app-test-compiler)
