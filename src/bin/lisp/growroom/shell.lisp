(fn unless (x . body)
  ^(? (not ,x)
      ,(mkbody body)))

(fn spin ()
  (skip-spaces)
  (in)
  (putback))

(fn read-line ()
  (unless (or (eof)
              (< (spin) 32))
    (. (read)
       (read-line))))

(fn exec (x)
  (assert x file?)
  (? (script? x)
     (run-script x)
     (run x)))

(fn sh ()
  (loop
    (? (== (spin) \()
       (eval (read))
       (exec (read-line))))))
