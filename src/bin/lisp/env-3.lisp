(load "when.lisp")
(load "let.lisp")
(load "progn.lisp")
(load "with.lisp")
(load "source.lisp")
(terpri)
(message "Welcome to TUNIX Lisp!")
(out "Left to LOAD: ")
(print '(adjoin alist dolist dotimes ensure-list group
         intersect max nthcdr prog1 queue queue-pop
         set-difference set-exclusive-or ;source stack
         subseq subseqp union unique unless while
         with-queue))
(terpri)
(message "(Append '.lisp' to filenames.)")
(print (gc))(out "B free.")(terpri)
(message "Ready.")
