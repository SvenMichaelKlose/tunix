(fn group (x size)
  (when x
    (cons (subseq x 0 size)
          (group (nthcdr size x) size))))
