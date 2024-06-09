; Essential tests before regular environment is loaded.

(out "Tests.")(terpri)
(out "ATOM")(terpri)
(atom 1)
(out "OR")(terpri)
(or nil t)
(or t nil)

; TODO: Test rest of built-ins.


(fn make-count (n)
  (? (< 0 n)
     (cons n (make-count (-- n)))))

; Not really a test.
(out "Recursion")(terpri)
(print (make-count 100))
(fresh-line)

; More of a speed test.
; 2024-06-09: VIC-20, x10,000 = 3:40min
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
