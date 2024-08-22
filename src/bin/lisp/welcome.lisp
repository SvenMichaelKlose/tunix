; Function called automatically after image load.
(fn istart ()
  (message "Ready."))

; Have the universe list compressed.
(= *universe (copy-list *universe*))
(message "Cleaning up. Please wait...")
(print (gc))(out " bytes free.")(terpri)
(and (builtin? 'time)
     (number? +bps+)
  (block t
    (out "Time since boot: ")
    (print (/ (- (time) *start-time*) +bps+))
    (out 's)
    (terpri)))

(message "Saving 'image'...")
(isave "image")
(message "Ready.")
