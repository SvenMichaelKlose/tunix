(load "do.lisp")
(terpri)
(message "Welcome to TUNIX Lisp!")
(out "Left to LOAD: ")
(print '(adjoin alist dolist dotimes ensure-list group
         intersect let max nthcdr prog1 queue queue-pop
         set-difference set-exclusive-or source stack
         source subseq subseqp union unique unless when
         while with-queue))
(terpri)
(message "(Append \".lisp\" to filenames.)")
(print (gc))(out "B free.")(terpri)
(message "Ready.")
