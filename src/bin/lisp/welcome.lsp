(load "pre-image.lsp")

(when (and (builtin? 'time)
           (number? +bps+))
  (out "Time since program start: ")
  (print (/ (- (time) *start-time*) +bps+))
  (out 's)
  (terpri)
  (= *universe* (remove '*start-time* *universe*)))

(unless (and (eq +target* 'unix)
             (builtin? isave))
  (fn istart ()
    (load "post-image.lsp")
    (message "Ready.")))

(when (builtin? gc)
  (message "Cleaning up. Please wait...")
  (= *macros* nil)
  (print (gc))(out " bytes free.")(terpri))

(or (eq +target* 'unix)
  (and (builtin? isave)
    ((()
       (message "Saving 'image'...")
       (isave "image")))))

(load "post-image.lsp")

(message "Ready.")
