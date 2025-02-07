(or (builtin? subseq)
  (fn subseq (l start . end)
    ;"Return the subsequence of LIST from START to END."
    ;"If END is nil, the subsequence goes to the end of the list."
    (with-queue q
      (do ((e end.)
           (i start (++! i))
           (l (nthcdr start l) .l))
          ((or (not l)
               (and e (>= i e))))
        (enqueue q l.)))))
