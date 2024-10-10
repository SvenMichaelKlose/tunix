(= *alv?* t) ; Verbose AUTOLOAD.
(app 'app-test-all)

(macro do-test (title . body)
  $((()
      (message ,title)
      (and (< (print (free)) 1024)
        ((()
           (fresh-line)
           (and (< (print (gc)) 4096)
             ((()
                (print 'env-reset)
                (fresh-line)
                (= *universe* (member 'app-test-all *universe*))
                (= *macros* (member-if '((x) (eq (car x) 'do-test))
                                       *macros*))))))))
      (fresh-line)
      ,@body)))

(do-test "Testing PROG1..."
  (or (equal (eval (macroexpand '(prog1 1 2 3)))
             1)
      (error 'PROG1)))

(do-test "Testing APROG1..."
  (or (equal (eval (macroexpand '(aprog1 1 2 3)))
             1)
      (error 'APROG1)))

(do-test "Testing PROGN..."
  (load "progn.lsp")
  (or (equal (macroexpand '(progn (error)))
             '(block t (error)))))

(do-test "Testing GROUP2..."
  (or (equal (group2 '(l i s p))
             '((l i) (s p)))
      (error (group2 '(l i s p)))))

(message "TODO: Testing UNLESS...")
(message "TODO: Testing WHEN...")
(message "TODO: Testing AWHEN...")

(do-test "Testing WITH..."
  (load "with.lsp")
  (or (equal (macroexpand '(with (a 1
                                  b 2)
                             (print a)
                             (print b)))
             '(((a b)
                 (print a)
                 (print b))
               1 2))
      (error "Expansion of WITH failed")))

(do-test "Testing WITH-GLOBAL..."
  (with (tmp 'dummy)
    (with-global tmp nil
      (and tmp
           (error "TMP is not NIL.")))
    (or tmp
        (error "TMP not restored."))))

(do-test "Testing PUSH and POP..."
  (or (with (x nil)
        (push 'p x)
        (push 's x)
        (push 'i x)
        (push 'l x)
        (and (equal x '(l i s p))
             (eq 'l (pop x))
             (equal x '(i s p))))
      (error "Test of PUSH/POP failed")))

(do-test "Testing DO..."
  (do ((i 0 (+ i 1)))
      ((>= i 10))
    (print i))
  (terpri))

(message "TODO: Testing MAKE-QUEUE...")
(message "TODO: Testing QUEUE-LIST...")
(do-test "Testing ENQUEUE..."
  (or (equal (with (q (make-queue))
               (enqueue q 42)
               (enqueue q 23)
               (queue-list q))
             '(42 23))
      (error "Expansion of ENQUEUE failed.")))
(message "TODO: Testing QUEUE-POP...")
(do-test "Testing WITH-QUEUE..."
  (or (equal (with-queue q
               (enqueue q 5))
             '(5))
      (error "Expansion of WITH-QUEUE failed")))

(do-test "Testing REVERSE..."
  (or (equal (reverse '(p s i l))
             '(l i s p))
      (error)))

(do-test "Smoke-testing DOLIST..."
  (dolist (i '(a b c d e f g h i j))
    (print i)))

(do-test "Smoke-testing DOTIMES..."
  (dotimes (i 10)
    (print i))
  (terpri))

(do-test "Smoke-testing WHILE..."
  (with (x 10)
    (while (< 0 x)
      (print x)
      (= x (-- x))))
  (terpri))

(do-test "Smoke-testing AWHILE..."
  (with (x 10)
    (awhile (< 0 x)
      (print x)
      (= x (-- x))))
  (terpri))

(do-test "Testing DUP..."
  (!= (dup 'x 0)
    (and ! (error !)))
  (!= (dup 'x 3)
    (or (equal '(x x x) !)
        (error !))))

(do-test "Testing MAPCAR..."
  (or (equal (mapcar + '(1 2 3) '(4 5 6))
             '(5 7 9))
      (error)))

(do-test "Testing MAPCAN..."
  (and (mapcan '((x)))
       (error))
  (and (mapcan '((x) x) '(nil))
       (error))
  (and (mapcan '((x) x) '(nil nil))
       (error))
  (or (equal (mapcan list '(l i s p))
             '(l i s p))
       (error)))

(do-test "Testing WITH*..."
  (load "with2.lsp")
  (!= (macroexpand '(with* (a 1
                            b 2)
                      (print a)
                      (print b)))
    (or (equal !
               '(((a b)
                   (= a 1)
                   (= b 2)
                   (print a)
                   (print b))
                 nil nil))
        (error "Expansion of WITH* failed"))))

(do-test "Testing CASE..."
  (case 23
    42 (error)
    23 'b
    (error))
  (case t
    42 (error)
    65 (error)
    'ok))

(do-test "Testing ADJOIN..."
  (or (equal (adjoin 'l '(i s p))
             '(l i s p))
      (error)))

(do-test "Testing !?..."
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
      (error)))

(do-test "Testing ASSOC..."
  (or (assoc 'vic '((c64 t)(vic t)))
      (error))
  (and (assoc 'vic '((c64 t)(amiga t)))
       (error)))

(do-test "Testing ACONS..."
  (or (with (x nil)
        (acons 'vic 20 x)
        (equal x '((vic . 20))))
      (error)))

(do-test "Testing AREMOVE..."
  (and (aremove t nil)
       (error))
  (or (equal (carlist (aremove 'i '((l) (i) (s) (p))))
             '(l s p))))

(do-test "Testing AREMOVE-IF..."
  (or (equal (carlist (aremove-if '((x) (eq 'i (car x)))
                                  '((l) (i) (s) (p))))
             '(l s p))))

(do-test "Testing COPY-TREE..."
  (or (equal (copy-tree '((1 2) (3 (4 5))))
             '((1 2) (3 (4 5))))
      (error)))

(do-test "Testing COUNT-IF..."
  (or (== 3 (count-if number? '(1 a 2 b 3)))
      (error)))

(do-test "Testing CUT-AT..."
  (with (head '(l i s p))
    (or (equal (cut-at 0 head)
               '(l i s p))
        (error))
    (or (equal (cut-at 2 head)
               '(s p))
        (error))
    (or (equal head
               '(l i))
        (error))))

(do-test "Testing ENSURE-LIST..."
  (or (equal '(tunix) (ensure-list 'tunix))
      (error))
  (or (equal '(tunix) (ensure-list '(tunix)))
      (error)))

(do-test "Testing EVERY..."
  (or (every '((x) (== x 1)) '(1 1 1 1))
      (error))
  (and (every '((x) (== x 1)) '(1 1 2 1))
      (error)))

(do-test "Testing FIND-IF..."
  (or (find-if '((x) (eq x 'i))
               '(l i s p))
      (error))
  (and (find-if '((x) (eq x 'x))
                '(l i s p))
       (error)))

(do-test "Testing FIND..."
  (or (find 'i '(l i s p))
      (error))
  (and (find 'x '(l i s p))
       (error)))

(do-test "Testing SUBSEQ..."
  (and (subseq '(l i s p) 0 0)
       (error))
  (or (equal (subseq '(l i s p) 0 2)
             '(l i))
      (error)))

(do-test "Testing GROUP..."
  (or (equal (group '(l i s p) 2)
             '((l i)
               (s p)))
      (error)))

(do-test "Testing !++..."
  (with (x 1)
    (!++ x)
    (or (== x 2)
        (error "X didn't increment to 2."))))

(do-test "Testing !--..."
  (with (x 1)
    (!-- x)
    (or (== x 0)
        (error "X didn't decrement to 0."))))

(message "TODO: Testing INTERSECT...")
(message "TODO: Testing MAX...")

(do-test "Testing MEMBER-IF..."
  (or (member-if '((x) (eq x 'i))
                   '(l i s p))
    (error))
  (and (member-if '((x) (eq x 'x))
                  '(l i s p))
       (error)))

(do-test "Testing NCONC..."
  (and (nconc)
       (error))
  (and (nconc nil)
       (error))
  (and (nconc nil nil)
       (error))
  (or (equal (nconc nil '(l i s p))
             '(l i s p))
       (error))
  (or (equal (nconc '(l i) '(s p))
             '(l i s p))
       (error))
  (or (equal (nconc '(l i) nil '(s p))
             '(l i s p))
       (error)))

(do-test "Testing NTHCDR..."
  ; TODO: Test with ONERROR.
  ;(and (nthcdr -1 '(l i s p))
  ;     (error))
  (or (equal (nthcdr 0 '(l i s p))
             '(l i s p))
      (error))
  (or (equal (nthcdr 2 '(l i s p))
             '(s p))
      (error))
  (and (nthcdr 4 '(l i s p))
       (error)))

(do-test "Testing NTH..."
  ; TODO: Test with ONERROR.
  ;(and (nth -1 '(l i s p))
       ;(error))
  (or (equal (nth 0 '(l i s p))
             'l)
      (error))
  (or (equal (nth 2 '(l i s p))
             's)
      (error))
  (and (nth 4 '(l i s p))
       (error)))

(do-test "Testing POSITION..."
  (and (position 'a '(l i s p))
       (error))
  (or (== 0 (position 'l '(l i s p)))
      (error))
  (or (== 2 (position 's '(l i s p)))
      (error)))

(message "TODO: Testing POSITION-IF...")
(message "TODO: Testing REMOVE-IF...")
(message "TODO: Testing SET-DIFFERENCE...")
(message "TODO: Testing SET-EXCLUSIVE-OR...")
(message "TODO: Testing SOURCE...")

(do-test "Testing SPLIT..."
  (and (split 'b nil)
       (error))
  (or (equal (split 'b '(a a a b a a b b a a a a))
             '((a a a) (a a) nil (a a a a)))))

(message "TODO: Testing SPLIT-IF...")

(do-test "Testing UNION..."
  (or (equal (union '(l l i i) '(s s p p))
             '(l i s p))
      (error)))

(do-test "Testing UNIQUE..."
  (or (equal (unique '(l l i i s s p p))
             '(l i s p))
      (error)))

(app 'app-test-all)
(rm-app 'app-test-all)
