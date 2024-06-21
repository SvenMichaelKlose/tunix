(print 'equal)(terpri)
(or (equal '(1 2) '(1 2))
    (error))

(print 'apply)(terpri)
(or (equal (apply '(x x) '(1))
           '(1))
    (error))
(or (equal (apply '(x x) '(1 2))
           '(1 2))
    (error))
(or (equal (apply '(x x) 1 2 '(3 4))
           '(1 2 3 4))
    (error))

(print 'eval)(terpri)
(or (equal (eval '(list 1 2 3))
           '(1 2 3))
    (error (eval '(list 1 2 3))))
(or (equal (eval '(list 1 2 ((nil 3))))
           '(1 2 3))
    (error (eval '(list 1 2 ((nil 3))))))

(print 'onerror)(terpri)

(var *last-err* nil)

(fn onerror (n repl ev)
  (out "onerror handler, errcode ")(print n)(terpri)
  (out "REPL: ")(print repl)(terpri)
  (out "Eval: ")(print ev)(terpri)
  (out "Continuing...")(terpri)
  (= *last-err* n)
  nil)

(fn terror (x)
  (= onerror nil)
  (error x))

; ERROR_TYPE          1
; ERROR_ARG_MISSING   2
; ERROR_TAG_MISSING   3
; ERROR_TOO_MANY_ARGS 4
; ERROR_NOT_FUNCTION  5
(not-a-function)
(or (== *last-err* 5)
    (terror "(not-a-function) should be an error."))
; ERROR_OUT_OF_HEAP   6
; ERROR_UNKNOWN_TYPE  7
; ERROR_NO_PAREN      8
; ERROR_STALE_PAREN   9
; ERROR_CHANNEL       10
; ERROR_FILE          11
; ERROR_USER          12

(= onerror nil)
