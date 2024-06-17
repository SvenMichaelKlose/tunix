(out "TUNIX Lisp (nightly)")(terpri)
(print (gc))(out "B free.")(terpri)

(load "smoke-test.lisp")
(load "equality.lisp")
(load "test.lisp")
(load "list.lisp")
(load "quasiquote.lisp")
(load "macroexpand.lisp")
(load "prog.lisp")

(out "Done loading.")(terpri)
(out "MACROEXPAND is not executed automatically in the ")
(out "REPL yet.")(terpri)

(var *files*
  '(let.lisp whenunless.lisp alist.lisp
    stack.lisp with.lisp while.lisp dolist.lisp set.lisp))
(out "Files not loaded: ")(print *files*)(terpri)

(print (gc))(out "B free.")(terpri)
