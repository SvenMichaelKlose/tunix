(fn socket-putc (s c)
  (socket-send s (symbol (list c))))
