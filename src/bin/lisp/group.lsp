(fn group (x size)
  (and x
       (. (subseq x 0 size)
          (group (nthcdr size x) size))))
