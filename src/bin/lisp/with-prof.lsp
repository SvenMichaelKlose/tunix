(and (builtin? 'time)
     (number? +bps+)
  (macro with-prof body
    (with (stime    (symbol)
           duration (symbol))
      $(let ,stime (time)
         (progn
           ,@body)
         (let ,duration (- (time) ,stime)
           (out "Time spent: ")
           (print (/ ,duration +bps+))
           (out 's)
           (out " (")
           (print ,duration)
           (out " bekloppies)")
           (terpri))))))
