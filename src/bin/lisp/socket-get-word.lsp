(fn socket-get-word (fd)
  (+ (socket-getc fd)
     (<< (socket-getc fd) 8)))
