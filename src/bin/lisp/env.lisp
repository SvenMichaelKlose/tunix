(out "TUNIX Lisp (nightly)")(terpri)
(print (gc))(out "B free.")(terpri)

(load "test.lisp")

; Avoid lengthy symbol look-ups.
(print (gc))(out "B free.")(terpri)

(fn eql (a b)
  (? (and (number? a)
          (number? b))
     (== a b)
     (eq a b)))

(load "list.lisp")
(load "quasiquote.lisp")
(load "macroexpand.lisp")
(load "macros.lisp")

(print (gc))(out "B free.")(terpri)
