(load "aif.lisp")
(load "awhen.lisp")
(load "member-if.lisp")
(load "find.lisp")
(load "find-if.lisp")
(load "adjoin.lisp")
(load "prog1.lisp")
(load "stack.lisp")
(load "alist.lisp")
(load "copy-tree.lisp")
(load "defsetfn.lisp")
(load "dolist.lisp")
(load "dotimes.lisp")
(load "ensure-list.lisp")
(load "queue.lisp")
(load "queue-pop.lisp")
(load "with-queue.lisp")
(load "remove-if.lisp")
(load "reverse.lisp")
(load "every.lisp")
(load "nth.lisp")
(load "when.lisp")
(load "incdec.lisp")
;(load "subseq.lisp")
(load "group.lisp")
(load "mapcar.lisp")
(load "mapcan.lisp")
(load "case.lisp")
(load "position.lisp")
(load "split.lisp")
(load "unique.lisp")
(load "union.lisp")
(load "intersect.lisp")
(load "max.lisp")
(load "progn.lisp")
(load "set-difference.lisp")
(load "set-exclusive-or.lisp")
(load "some.lisp")
(load "source.lisp")
(load "unless.lisp")
(load "with-global.lisp")
(load "while.lisp")
(load "awhile.lisp")
(message "All loaded.")
(message "Cleaning up. Please wait...")
; Have the universe list compressed.
(= *universe* (copy-list *universe*))
(print (gc))(out " bytes free.")(terpri)
