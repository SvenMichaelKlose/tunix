(fresh-line)
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
; Knock off some extra compression of conses.
(= *universe (copy-list *universe*))
(print (gc))(message " bytes free.")
(and (builtin? 'time)
     (number? +bps+)
  (block t
    (out "Time since boot: ")
    (print (/ (- (time) *start-time*) +bps+))
    (out 's)
    (terpri)))
(message "Ready.")
