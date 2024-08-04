(load "progn.lisp")
(load "let.lisp")
(load "with.lisp")
(load "do.lisp")

(message "Welcome to TUNIX Lisp!")
(out "LOADables: (Append \".lisp\"!)")(terpri)
(print '(adjoin alist all copy-tree do dolist dotimes
         ensure-list equality every find-if find group
         intersect let list max member-if nthcdr prog1 progn
         queue queue-pop remove-if reverse set-difference
         set-exclusive-or smoke-test some source stack
         subseq test test-onerror union unique unless when
         while with with-queue))
(terpri)
(print (gc))(message " bytes free.")
(message "Ready.")
