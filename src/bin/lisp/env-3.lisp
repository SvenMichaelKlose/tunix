(load "progn.lisp")
(load "let.lisp")
(load "with.lisp")
(load "do.lisp")
(terpri)
(message "Welcome to TUNIX Lisp!")
(out "LOADables: (Append \".lisp\"!)")(terpri)
(print '(adjoin alist copy-tree do dolist dotimes
         ensure-list equality every find-if find group
         intersect max member-if nthcdr prog1 progn queue
         queue-pop set-difference set-exclusive-or some
         source stack subseq subseqp union unique unless
         when while with-queue))
(terpri)
(print (gc))(message "B free.")
(message "Ready.")
(load "all.lisp")
