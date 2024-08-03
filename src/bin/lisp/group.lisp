(or (macro? 'when)
    (load "when.lisp"))
(or (cons? subseq)
    (load "subseq.lisp"))
(or (cons? nthcdr)
    (load "nthcdr.lisp"))

(fn group (x size)
  (and x
       (cons (subseq x 0 size)
             (group (nthcdr size x) size))))

(message "Testing GROUP...")
(or (equal (group '(l i s p) 2)
           '((l i)
             (s p)))
    (error))
