; Function called automatically after image load.
(fn istart ()
  (load "user-post-image.lisp")
  (message "Ready."))

; Load user file.
(or (load "user-pre-image.lisp")
    (message "INFO: You can create a file called \"user-pre-image.lisp\" to be loaded here."))

(and (builtin? gc)
  ((()
     (message "Cleaning up. Please wait...")
     ; If compressed conses are enabled, have *UNIVERSE*
     ; compressed better.
     (= *universe (copy-list *universe*))
     (print (gc))(out " bytes free.")(terpri))))

; Tell time it took to get here.
(and (builtin? 'time)
     (number? +bps+)
  (block t
    (out "Time since boot: ")
    (print (/ (- (time) *start-time*) +bps+))
    (out 's)
    (terpri)))

(or (eq +target* 'unix)
  (block t
    (message "Saving 'image'...")
    (isave "image")))

; Load user image.
(or (load "user-post-image.lisp")
    (message "INFO: You can create a file called \"user-post-image.lisp\" to be loaded here and when the image is loaded on program start."))

(message "Ready.")
