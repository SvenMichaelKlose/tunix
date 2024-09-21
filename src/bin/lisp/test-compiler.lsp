(or (macro? '!?=)
    (load "aif.lsp"))
(or (cons? mapcar)
    (load "mapcar.lsp"))
(or (cons? app)
    (load "app.lsp"))

(app 'app-test-compiler)

(message "Testing CMACROEXPAND...")
(load "cmacros.lsp")
(print (cmacroexpand '(and a b c)))
(print (cmacroexpand '(or a b c)))
(print (cmacroexpand '(? a b c)))

(message "Testing ARGEXPAND...")
(load "argexpand.lsp")
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
(load "fold-block.lsp")
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
(load "inline-fn.lsp")
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

;(message "Testing EXEXPAND...")
;(load "exexpand.lsp")
;(print (exexpand (print '(a b c d))))
;(print (exexpand (print '(a (b c d)))))
;(print (exexpand (print '(a (b (c d))))))

(load "bcasm.lsp")

(message "Testing COMPILE...")
(load "dotimes.lsp")
(fn compile (x)
  (message "In:")
  (print x)
  (message "MACROEXPAND:")
  (= x (macroexpand x))
  (print x)
  (message "CMACROEXPAND:")
  (= x (cmacroexpand x))
  (print x)
  (message "INLINE-FN:")
  (= x (@ inline-fn x))
  (print x)
  (message "FOLD-BLOCK:")
  (= x (mapcan fold-block x))
  (print x)
  )
  ;(print 'exexpand)
  ;(= x (mapcan exexpand x))
  ;(print x))

(print (length (compile (cdr fold-block))))

(app 'app-test-compiler)
