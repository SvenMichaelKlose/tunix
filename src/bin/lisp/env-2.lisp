(load '"quasiquote.lisp")
(load '"macroexpand.lisp")

(out '"Cleaning up. Please wait...")(terpri)
(print (gc))(out '"B free.")(terpri)
