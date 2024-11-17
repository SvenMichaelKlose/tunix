(fn reset! ()
  (= *universe* (member 'reset! *universe*))
  (= *macros* nil))
