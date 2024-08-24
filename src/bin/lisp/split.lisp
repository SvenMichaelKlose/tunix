(or (cons? subseq)
    (load "subseq.lisp"))
(or (cons? position)
    (load "position.lisp"))

(fn split (x l . f)
  (= f (? f (car f) eql))
  (and l
       (!? (position x l f)
           (cons (subseq l 0 !)
                 (split x (subseq l (++ !)) f))
           (list l))))

(message "Testing SPLIT...")
(and (split 'b nil)
     (error))
(or (equal (split 'b '(a a a b a a b b a a a a))
           '((a a a) (a a) nil (a a a a))))
