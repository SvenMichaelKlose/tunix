(var *reqid* 3735928559) ; 0xdeadbeef

(var +lse-load+ 1)
(var +lse-store+ 2)
(var +lse-exec+ 4)

(fn word-bytes (x)
  (list (bit-and 255 x)
        (>> x 8)))

(fn dword-bytes (x)
  (list (bit-and 255 x)
        (bit-and 255 (>> x 8))
        (bit-and 255 (>> x 16))
        (bit-and 255 (>> x 24))))

(fn vice-send (fd cmd body)
  (socket-send fd
    (symbol $(,reqid
              ,@(word-bytes (length body))
              ,@(word-bytes *reqid*)
              ,cmd
              ,@body))))

(fn vice-resp (fd type)
  (or (== 2 (socket-getc fd))
      (error "STX"))
  (or (== 2 (socket-getc fd))
      (error "API version"))
  (return
    $((len  . ,(socket-get-le-word fd))
      (type . ,(aprog1 (socket-getc fd)
                 (unless (== type !)
                   (error "Response type is " ! '". "
                          type '" expected."))))
      (err  . ,(socket-getc fd))
      (id   . ,(socket-get-le-word fd)))))

(fn vice-memget (fd side? start end memspace bank)
  (vice-send fd 1
             $(,(vice-word start)
               ,(vice-word end)
               ,memspace
               ,bank))
  (!= (vice-resp fd 1)
    (return symbol-name (socket-getn fd len)))

(fn vice-memset (fd side? start end memspace bank data)
  (vice-send fd 2
             $(,@(word-bytes start) ,@(word-bytes end)
               ,memspace ,bank ,@data))
  (vice-resp fd 2))

(fn vice-chkpt-resp (fd)
  (when (cdr (assoc 'body (vice-resp fd 17)))
    $((n        . ,(socket-get-dword fd))
      (hit?     . ,(socket-getc fd))
      (start    . ,(socket-get-word fd))
      (end      . ,(socket-get-word fd))
      (break?   . ,(socket-getc fd))
      (enabled? . ,(socket-getc fd))
      (lse?     . ,(socket-getc fd))
      (tmp      . ,(socket-getc fd))
      (nhits    . ,(socket-get-dword fd))
      (nignored . ,(socket-get-dword fd))
      (cond?    . ,(socket-get-dword fd))
      (memspace . ,(socket-getc fd)))))

(fn vice-chkpt-get (fd n)
  (vice-send fd 17 (list n))
  (vice-chkpt-resp fd))

(fn vice-chkpt-set (fd start end break? enabled? lse? tmp? memspace)
  (vice-send fd 18
             $(,@(word-bytes start) ,@(word-bytes end)
               ,break? ,enabled? ,lse? ,tmp? ,memspace))
  (vice-chkpt-resp fd))
