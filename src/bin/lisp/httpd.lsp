; ⚠️  UNDER CONSTRUCTION ⚠️
; ⚠️  NEVER TESTED ⚠️
; ⚠️  JUST BEAUTIFUL ⚠️

;(require 'let 'progn 'while 'do '!=
;         'unless 'let* 'dup 'aprog1
;         'dotimes 'push 'split '!?
;         'position)

(fn send-response-header (mime)
  (out "HTTP/1.1 200 OK" 13 10
       "Content-Type: " mime 13 10
       13 10))

(fn http-error (code . msg)
  (out "HTTP/1.1 ")
  (print code)
  (out " " msg 13 10
       "Content-Type: " mime 13 10
       13 10))

(fn http-get (param l)
  (awhen (with-in i (open param 'r)
           (send-response-header mime)
           (awhile (in)
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
  (let* (cmd     (split \  (read-line))
         method  cmd.
         param   .cmd)
    (out "Received request: " request)
    (parse-req-info)
    (!? (slot-value *methods* method)
        (funcall ! param)
        (http-error 666 "Unknown method " method))))

(fn http-readline (socket)
  (let (oldin fnin)
    (setin socket)
    (prog1 (read-line)
      (setin oldin))))

(fn httpd-serve (socket)
  (!= (socket-accept socket)
    (when (err)
      (error "Sock accept: " (.. (errsym))))
    (message "Accepting connection on socket " (.. !))
    (let (oldout fnout
          txt "Hello world!")
      (setout !)
      (out txt)
      (terpri)
      (when (err)
        (error "Conn out: " (.. (errsym))))
      (setout oldout)
      (socket-close !))))

(fn httpd (port)
  (let (socket (socket-listen port))
    (when (err)
      (error "Sock listen: " (.. (errsym))))
    (message "Socket " (.. socket) " listening on port " (.. port))
    (httpd-serve socket)
    (socket-close socket))
  (message "httpd exiting"))
