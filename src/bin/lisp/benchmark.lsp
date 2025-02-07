(message "TUNIX Lisp benchmark")

(or (builtin? 'time)
    (error "(TIME) missing"))
(or (number? +bps+)
    (error "+BPS+ missing"))

(reset!)
(gc)
(!= (> 8192 (free))
  (out "WARNING: 8K free heap required, but only ")
  (print !) (out '" bytes free.") (terpri))

(message '"Please wait...")

(fn block-test (c)
  (block nil
    tag
    (= c (-- c))
    (? (== 0 (% c 100))
       (print c))
    (? (not (== c 0))
       (go tag))))

; Have WITH-PROF and requirements on the heap.
(with-prof nil)

(!= (progn
      (gc)
      (message '"Test: Basic GC time...")
      (with-prof (gc))
      (message '"Test: BLOCK countdown...")
      (with-prof (block-test 1000)))
  (print (/ (* 8 1000) (/ ! +bps+)))
  (out '" calls/s.")
  (terpri))
(message '"Test: GC time...")
(with-prof (gc))
