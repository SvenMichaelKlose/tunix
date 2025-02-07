(fn socket-getc (n)
  ;"Read char from socket."
  (!= (or (assoc n nil *sockq*)
          (acons! n nil *sockq*))
    (unless .!
      (=-cdr ! (symbol-name (socket-recv n))))
    (prog1 .!.
      (=-cdr ! ..!))))
