(or (cons? nthcdr)
    (load "nthcdr.lisp"))
(or (cons? make-queue)
    (load "queue.lisp"))

(fn subseq (l start . end)
  ;"Return the subsequence of LIST from START to END.  If END is nil, the subsequence goes to the end of the list."
  (do ((q (make-queue))
       (i (- (or (car end) (length l)) start) (-- i))
       (x (nthcdr start l) (cdr x)))
      ((<= i 0) (queue-list q))
    (enqueue q (car x)))

(message "Testing SUBSEQ...")
(or (equal (print (subseq '(l i s p) 0 2))
           '(l i))
    (error))
