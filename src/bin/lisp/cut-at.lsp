(fn cut-at (n l)
  ;"Destructively cut L at position N and return the cut off tail."
  (? (== n 0)
     l
     (let* (end  (nthcdr (-- n) l)
            next (cdr end))
       (and end (setcdr end nil))
       next)))
