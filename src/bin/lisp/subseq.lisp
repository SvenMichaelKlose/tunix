(or (builtin? subseq)
  (fn subseq (l start . end)
    ;"Return the subsequence of LIST from START to END."
    ;"If END is nil, the subsequence goes to the end of the list."
    (with-queue q
      (do ((e (car end))
           (i start (!++ i))
           (l (nthcdr start l) (cdr l)))
          ((or (not l)
               (and e (>= i e))))
        (enqueue q (car l))))))
