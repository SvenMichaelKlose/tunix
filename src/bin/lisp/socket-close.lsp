(fn socket-close (n)
  (%sclose n)
  (unless (eq *sockq* '*sockq*)
    (= *sockq* (aremove n *sockq*))))
