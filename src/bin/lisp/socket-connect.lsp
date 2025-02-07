(or (member '*sockq* *universe*)
    (var *sockq* nil))

(fn socket-connect (s n)
  (%sconnect s n))
