(load "progn.lisp")
(load "let.lisp")
(load "with.lisp")
(load "do.lisp")
(terpri)
(message "Welcome to TUNIX Lisp!")
(out "LOADables: (Append \".lisp\"!)")(terpri)
(print '(adjoin alist dolist dotimes ensure-list group
         intersect let max nthcdr prog1 queue queue-pop
         set-difference set-exclusive-or source stack
         source subseq subseqp union unique unless when
         while with-queue))
(terpri)
(print (gc))(out "B free.")(terpri)
(message "Ready.")
(load member-if.lisp)
