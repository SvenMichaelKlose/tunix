(macro while (test . body)
  $(do ()
       ((not ,test))
     ,@body))

(message "Testing WHILE...")
(let x 10
  (while (< 0 x)
    (print x)
    (= x (-- x))))
(terpri)
