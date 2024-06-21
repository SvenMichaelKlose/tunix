(fn group (x size)
  (when x
    (cons (list-subseq x 0 size)
          (group (nthcdr size x) size))))
