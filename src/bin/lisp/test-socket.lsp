(!? (socket-connect "127.0.0.1" 8000)
    (progn
      (socket-send ! (symbol (append (symbol-name "GET / HTTP/1.1")
                                     (list 13 10 13 10))))
      (print (socket-recv !))
      (socket-close !))
    (message "Cannot open 127.0.0.1:8000."))
