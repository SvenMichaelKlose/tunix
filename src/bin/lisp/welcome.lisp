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
