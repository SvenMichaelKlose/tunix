(fn pipe ()
  (!= '(())
    (gopen $((setin  . ((ch)))
             (setout . ((ch)))
             (in     . (()   (queue-pop ,!)))
             (conin  . (()   (queue-pop ,!)))
             (out    . ((c)  (enqueue ,! c)))
             (eof    . (()   (not (cdr ,!))))
             (close  . ((ch) (cdr ,!)))
             (err    . (()))))))