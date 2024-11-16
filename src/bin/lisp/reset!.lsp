(fn reset! ()
  (= *universe* (cdr (member 'reset! *universe*)))
  (= *macros* nil))
