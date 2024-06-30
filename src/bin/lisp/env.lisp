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

; Required by a compiler.
(load '"let.lisp")
(load '"with.lisp")
(load '"dolist.lisp")
(load '"prog1.lisp")
(load '"progn.lisp")
(load '"unless.lisp")
(load '"when.lisp")
(load '"while.lisp")
(load '"nthcdr.lisp")
(load '"alist.lisp")
(load '"stack.lisp")
(load '"queue.lisp")
(load '"set.lisp")
(load '"max.lisp")
(load '"subseq.lisp")
(load '"group.lisp")

(terpri)
(out '"Cleaning up. Please wait...")(terpri)
(print (gc))(out '"B free.")(terpri)
;(= x (macroexpand '(() (dolist (i '(1 2 3)) (print i)))))
;(debugger)
;(eval x)
