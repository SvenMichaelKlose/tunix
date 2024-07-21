((()))
(out '"TUNIX Lisp (nightly)")(terpri)

; Wire everything and watch out for sparks.
(load '"smoke-test.lisp")

(fn identity (x) x)
(load '"equality.lisp")
(load '"list.lisp")

; Test what's around so far.
(load '"test.lisp")

(load '"quasiquote.lisp")
(load '"macroexpand.lisp")

(out '"Cleaning up. Please wait...")(terpri)
(print (gc))(out '"B free.")(terpri)
