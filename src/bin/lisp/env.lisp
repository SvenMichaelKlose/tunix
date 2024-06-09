(out "TUNIX Lisp (nightly)")(terpri)

(print (gc))(out "B free.")(terpri)

(load "test.lisp")
; Avoid lengthy symbol look-ups.
(fresh-line)(print (gc))(out "B free.")(terpri)

(fn eql (a b)
  (? (and (number? a)
          (number? b))
     (== a b)
     (eq a b)))

(load "list.lisp")
(load "macro.lisp")

(fresh-line)(print (gc))(out "B free.")(terpri)
