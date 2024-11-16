(load 'pre-image.lsp)

(and (builtin? 'time)
     (number? +bps+)
  ((()
     (out "Time since program start: ")
     (print (/ (- (time) *start-time*) +bps+))
     (out 's)
     (terpri)
     (= *universe* (remove '*start-time* *universe*)))))

(and (builtin? gc)
  ((()
     (message "Cleaning up. Please wait...")
     (= *macros* nil)
     (print (gc))(out " bytes free.")(terpri))))

(and (builtin? isave)
  ((()
     (message "Saving 'image'...")
     (isave 'image))))

(load 'post-image.lsp)

(message '"Ready.")
