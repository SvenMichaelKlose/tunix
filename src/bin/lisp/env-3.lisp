(load "progn.lisp")
(load "let.lisp")
(load "with.lisp")
(load "do.lisp")
(terpri)
(message "Welcome to TUNIX Lisp!")
(out "LOADables: (Append \".lisp\"!)")(terpri)
(print '(adjoin alist all copy-tree dolist dotimes
         ensure-list every find-if find group intersect max
         member-if nthcdr prog1 progn queue queue-pop
         remove-if reverse set-difference set-exclusive-or
         smoke-test some source stack subseq test
         test-onerror union unique unless when while
         with-queue))
(terpri)
(print (gc))(message "B free.")
(message "Ready.")
