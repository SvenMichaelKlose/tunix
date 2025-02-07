(fn socket-close (n)
  (= *sockq* (aremove n *sockq*))
  (%sclose n))
