(message "Testing ADJOIN...")
(or (equal (adjoin 'l '(i s p))
           '(l i s p))
    (error))

(message "Testing !?...")
(!? 49
    (or (eql ! 49)
        (error)))
(!?
  nil (error)
  49  (or (eql ! 49)
          (error)))
(or (eql 5 (!? nil
               (error)
               5))
    (error))
(or (cons? find-if)
    (load "find-if.lisp"))
(or (macro? 'push)
    (load "stack.lisp"))

(message "Testing ASSOC...")
(or (assoc 'vic '((c64 t)(vic t)))
    (error))
(and (assoc 'vic '((c64 t)(amiga t)))
     (error))

(message "Testing ACONS...")
(or (let x nil
      (acons 'vic 20 x)
      (equal x '((vic . 20))))
    (error))

(message "TODO: Testing AWHEN...")

(message "Testing CASE...")
(case 23
  42 (error)
  23 'b
  (error))
(case t
  42 (error)
  65 (error)
  'ok)

(message "Testing COPY-TREE...")
(or (equal (copy-tree '((1 2) (3 (4 5))))
           '((1 2) (3 (4 5))))
    (error))
(macro defsetfn (name . body)
  $(fn ,name (a b)
     ,@body ))

(message "Testing DO...")
(do ((i 0 (+ i 1)))
    ((>= i 10))
  (print i))
(terpri)

(message "Testing DOLIST...")
(dolist (i '(1 2 3 4 5 6 7 8 9 10) (terpri))
  (terpri)(out '"Item ")(print i))

(message "Testing DOTIMES...")
(dotimes (i 10 (terpri))
  (print i))

(message "Testing ENSURE-LIST...")
(or (equal '(tunix) (ensure-list 'tunix))
    (error))
(or (equal '(tunix) (ensure-list '(tunix)))
    (error))

(message "Testing EVERY...")
(or (every '((x) (== x 1)) '(1 1 1 1))
    (error))
(and (every '((x) (== x 1)) '(1 1 2 1))
    (error))
(or (cons? member-if)
    (load "member-if.lisp"))

(message "Testing FIND-IF...")
(or (find-if '((x) (eq x 'i))
             '(l i s p))
    (error))
(and (find-if '((x) (eq x 'x))
              '(l i s p))
     (error))

(message "Testing FIND...")
(or (find 'i '(l i s p))
    (error))
(and (find 'x '(l i s p))
     (error))
(or (macro? 'when)
    (load "when.lisp"))
(or (cons? subseq)
    (load "subseq.lisp"))
(or (cons? nthcdr)
    (load "nthcdr.lisp"))

(message "Testing GROUP...")
(or (equal (group '(l i s p) 2)
           '((l i)
             (s p)))
    (error))

(message "TODO: Testing GROUP...")

(message "Testing MACRO? on LET...")
(or (macro? 'let)
    (error))

(message "Testing LET...")
(or (equal (macroexpand '(let a 1
                           (+ 2 a)))
           '(((a)
               (+ 2 a))
             1))
    (error))

(message "Testing MAPCAN...")
(and (mapcan '((x)))
     (error))
(and (mapcan '((x) x) '(nil))
     (error))
(and (mapcan '((x) x) '(nil nil))
     (error))
(or (equal (mapcan list '(l i s p))
           '(l i s p))
     (error))

(message "Testing MAPCAR...")
(or (equal (mapcar + '(1 2 3) '(4 5 6))
           '(5 7 9))
    (error))

(message "TODO: Testing MAX...")

(message "Testing MEMBER-IF...")
(or (member-if '((x) (eq x 'i))
               '(l i s p))
    (error))
(and (member-if '((x) (eq x 'x))
                '(l i s p))
     (error))

(message "Testing NTHCDR...")
(and (nthcdr -1 '(l i s p))
     (error))
(or (equal (nthcdr 0 '(l i s p))
           '(l i s p))
    (error))
(or (equal (nthcdr 2 '(l i s p))
           '(s p))
    (error))
(and (nthcdr 4 '(l i s p))
     (error))

(message "Testing NTH...")
(and (nth -1 '(l i s p))
     (error))
(or (equal (nth 0 '(l i s p))
           'l)
    (error))
(or (equal (nth 2 '(l i s p))
           's)
    (error))
(and (nth 4 '(l i s p))
     (error))
(or (macro? '!?)
    (load "aif.lisp"))

(message "Testing POSITION...")
(and (position 'a '(l i s p))
     (error))
(or (== 0 (position 'l '(l i s p)))
    (error))
(or (== 2 (position 's '(l i s p)))
    (error))

(message "Testing PROG1...")
(or (equal (eval (macroexpand '(prog1 1 2 3)))
           1)
    (error (eval (macroexpand '(prog1 1 2 3)))))

(message "Testing PROGN...")
(or (equal (macroexpand '(progn (error)))
           '(block t (error))))

(message "TODO: Testing MAKE-QUEUE...")

(message "TODO: Testing QUEUE-LIST...")

(message "Testing ENQUEUE...")
(or (equal (let q (make-queue)
             (enqueue q 42)
             (enqueue q 23)
             (queue-list q))
           '(42 23))
    (error))

(message "TODO: Testing QUEUE-POP...")

(message "TODO: Testing REMOVE-IF...")

(message "Testing REVERSE...")
(or (equal (reverse '(p s i l))
           '(l i s p))
    (error))
(or (macro? 'defsetfn)
    (load "defsetfn.lisp"))

(message "TODO: Testing SET-DIFFERENCE...")

(message "TODO: Testing SET-EXCLUSIVE-OR...")

(message "TODO: Testing SOME...")

(message "TODO: Testing SOURCE...")

(message "Testing SPLIT...")
(and (split 'b nil)
     (error))
(or (equal (split 'b '(a a a b a a b b a a a a))
           '((a a a) (a a) nil (a a a a))))
(or (macro? 'prog1)
    (load "prog1.lisp"))

(message "Testing PUSH and POP...")
(or (let x nil
      (push p x)
      (push s x)
      (push i x)
      (push l x)
      (and (equal x '(l i s p))
           (eq 'l (pop x))
           (equal x '(i s p))))
    (error))
(or (cons? nthcdr)
    (load "nthcdr.lisp"))
(or (cons? make-queue)
    (load "queue.lisp"))

(message "Testing SUBSEQ...")
(or (equal (subseq '(l i s p) 0 2)
           '(l i))
    (error))

(message "Testing UNION...")
(or (equal (union '(l l i i) '(s s p p))
           '(l i s p))
    (error))

(message "Testing UNIQUE...")
(or (equal (unique '(l l i i s s p p))
           '(l i s p))
    (error))

(message "TODO: Testing UNLESS...")

(message "TODO: Testing WHEN...")

(message "Testing WHILE...")
(let x 10
  (while (< 0 x)
    (print x)
    (= x (-- x))))
(terpri)

(message "Testing WITH-GLOBAL...")
(let old-macros *macros*
  (with-global *macros* nil
    (and *macros*
         (error "*MACROS* not NIL.")))
    (or *macros*
        (error "*MACROS* is NIL.")))

(message "Testing WITH...")
(or (equal (macroexpand '(with ((a 1)
                                (b 2))
                           (print a)
                           (print b)))
           '(((a b)
               (print a)
               (print b))
             1 2))
    (error))

(message "TODO: Testing WITH-QUEUE...")
