((()))
(out '"TUNIX Lisp (nightly)")(terpri)

; Wire everything and watch out for sparks.
(load '"smoke-test.lisp")

(load '"equality.lisp")
(load '"list.lisp")

; Test what's around so far.
(load '"test.lisp")

(load '"quasiquote.lisp")
(load '"macroexpand.lisp")

(load '"let.lisp")
(load '"prog1.lisp")
(load '"progn.lisp")
(load '"with.lisp")
(load '"alist.lisp")
(load '"dolist.lisp")
(load '"set.lisp")
(load '"stack.lisp")
(load '"unless.lisp")
(load '"when.lisp")
(load '"while.lisp")
(load '"max.lisp")
(load '"nthcdr.lisp")
(load '"queue.lisp")
(load '"subseq.lisp")
(load '"group.lisp")

(terpri)
(out '"Cleaning up. Please wait...")(terpri)
(print (gc))(out '"B free.")(terpri)
(= x (macroexpand '(() (dolist (i '(1 2 3)) (print i)))))
(debugger)
(eval x)
