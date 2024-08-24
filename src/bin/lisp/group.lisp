(fn group (x size)
  (and x
       (cons (subseq x 0 size)
             (group (nthcdr size x) size))))
