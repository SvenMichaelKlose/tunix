(fresh-line)
(out "LOADables: (Append \".lisp\"!)")(terpri)
(print '(adjoin alist all copy-tree do dolist dotimes
         ensure-list equality every find-if find group
         intersect let list max member-if nthcdr prog1 progn
         queue queue-pop remove-if reverse set-difference
         set-exclusive-or smoke-test some source stack
         subseq test test-onerror union unique unless when
         while with with-queue))
(terpri)

; Function called automatically after image load.
(fn istart ()
  (message "Ready."))

; Have the universe list compressed.
(= *universe (copy-list *universe*))
(message "Cleaning up. Please wait...")
(print (gc))(message " bytes free.")
(and (builtin? 'time)
     (number? +bps+)
  (block t
    (out "Time since boot: ")
    (print (/ (- (time) *start-time*) +bps+))
    (out 's)
    (terpri)))

;TODO: (message "Saving 'image'...")
;(isave "image")
(message "Ready.")
