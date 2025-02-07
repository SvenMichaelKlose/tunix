(fn socket-getc (n)
  ;"Read char from socket."
  (!= (or (assoc n nil *sockq*)
          (acons! n nil *sockq*))
    (unless .!
      (setcdr ! (symbol-name (socket-recv n))))
    (prog1 .!.
      (setcdr ! ..!))))
