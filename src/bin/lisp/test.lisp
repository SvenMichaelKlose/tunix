(out "Tests.")(terpri)
(out "ATOM")(terpri)
(atom 1)
(out "OR")(terpri)
(or nil t)
(or t nil)

(fn make-count (n)
  (? (< 0 n)
     (cons n (make-count (-- n)))))

(out "Recursion (MAKE-COUNT 200)")(terpri)
(print (make-count 100))
(fresh-line)

(fn block-test (c)
  (out "Looping ")
  (print c)
  (out " times...")
  (terpri)
  (block nil
    tag
    (= c (-- c))
    (? (== 0 (% c 100))
       (print c))
    (? (not (== c 0))
       (go tag))))

(out "Loop (BLOCK-TEST 1000)")(terpri)
(block-test 300)
(fresh-line)
