(fn socket-get-dword (fd)
  (+ (socket-get-word fd)
     (<< (socket-get-word fd) 16)))
