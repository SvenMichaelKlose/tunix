; Function called automatically after image load.
(fn istart ()
  (load "post-image.lsp")
  (message "Ready."))

(load "pre-image.lsp")

(and (builtin? gc)
  (block t
    (message "Cleaning up. Please wait...")
    ; If compressed conses are enabled, have *UNIVERSE*
    ; compressed better.
    (print (gc))(out " bytes free.")(terpri)))

(or (eq +target* 'unix)
    (? (builtin? isave)
       (block t
         (message "Saving 'image'...")
         (isave "image"))))

; Tell time it took to get here.
(and (builtin? 'time)
     (number? +bps+)
  (block t
    (out "Time since boot: ")
    (print (/ (- (time) *start-time*) +bps+))
    (out 's)
    (terpri)
    (= *universe* (remove '*start-time* *universe*))))

(load "post-image.lsp")

(message "Ready.")
