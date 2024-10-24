(fn socket-getc (n)
  ;"Read char from socket."
  (!= (or (assoc n *sockq*)
          (aprog1 (. n nil)
            (push ! *sockq*)))
    (unless (cdr !)
      (setcdr ! (symbol-name (socket-recv !))))
    (prog1 (cadr !)
      (setcdr ! (cddr !)))))
