((()))
(out '"TUNIX Lisp (nightly)")(terpri)

; Wire everything and watch out for sparks.
(load '"smoke-test.lisp")

(fn identity (x) x)
(load '"equality.lisp")
(load '"list.lisp")
