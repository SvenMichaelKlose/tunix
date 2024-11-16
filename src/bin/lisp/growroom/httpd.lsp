; ⚠️  UNDER CONSTRUCTION ⚠️

(fn send-response-header (mime)
  (out "HTTP/1.1 200 OK" 13 10
       "Content-Type: " mime 13 10
       13 10))

(fn http-error (code . msg))

(fn http-get (param l)
  (awhen (with-in i (open param 'r)
           (send-response-header mime)
           (awhile (reads)
             (? (eq ! *eof*)
                (return))
             (out !)))
    (http-error 404 "not found")))

(var *methods*
     '((GET . http-get)))
(var *reqinfo* nil)

(fn parse-req-info ()
  (= *reqinfo* nil)
  (awhile (read-line)
    (? (== 0 (slength !))
       (return))
    (let (item (split \: !))
      (acons! item. (apply append (pad \: .item))
              *reqinfo*))))

(fn handle-request ()
  (out "Received request: " request)
  (let* (cmd     (split \  (read-line))
         method  cmd.
         param   .cmd)
    (parse-req-info)
    (!? (slot-value *methods* method)
        (funcall ! param)
        (http-error 666 "Unknown method " method))))

(fn http-server (port)
  (let (socket (socket-listen port))
    (awhile (socket-accept socket)
      (let (oldin fnin)
        (setin !)
        (setout !)
        (handle-request)
        (socket-close !)
        (setin oldin)))))

(http-server 8080)
