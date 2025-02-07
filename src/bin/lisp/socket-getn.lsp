(fn socket-getcn (fd n)
  ;"Read n chars from socket."
  (with-queue q
    (dotimes (i n)
      (enqueue q (socket-getc fd)))))
