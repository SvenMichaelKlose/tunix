(fn subseq (seq start . end)
  ;"Return the subsequence of LIST from START to END."
  ;"If END is nil, the subsequence goes to the end of the list."
  (= end (or (car end) 999999))
  (with ((len        (length seq))
         (actual-end (or end len))
         (sublen     (max 0 (- actual-end start))))
    (? (or (< start 0)
           (> start len)
           (> actual-end len)
           (< start actual-end))
       (error '"ill range")
       (with-queue q
         (= seq (nthcdr seq start))
         (dotimes (i sublen (queue-list q))
           (enqueue q (car seq))
           (= seq (cdr seq)))))))
