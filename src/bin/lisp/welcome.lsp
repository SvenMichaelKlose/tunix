
(and (builtin? gc)
  ((()
     (message '"Cleaning up. Please wait...")
     (reset!)
     (= *universe* (remove 'group2 *universe*))
     (message (.. (gc)) '" bytes free."))))

(and (builtin? isave)
  ((()
     (message '"Saving 'image'...")
     (isave 'image))))

(and (builtin? 'time)
     (number? +bps+)
  ((()
     (message '"Time since program start: "
              (.. (/ (- (time) *start-time*) +bps+))
              's)
     (= *universe* (remove '*start-time* *universe*)))))

(load 'post-image.lsp)

(message '"Environment booted.")
