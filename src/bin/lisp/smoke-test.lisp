(var x nil)

(fn message (x)
  (out x)
  (terpri))

(message "Testing GC...")
(print (gc))(out '"B free.")

(message "Testing ATOM...")
(atom 1)

(message "Testing AND...")
(and nil (error))

(message "Testing OR...")
(or t (error))
(or nil t (error))

(message "Smoke-testing COPY-LIST...")
(print (copy-list nil))(terpri)
(print (copy-list '(1)))(terpri)
(print (copy-list '(1 2 3)))(terpri)

(message "Smoke-testing APPLY...")
(print (apply '(x x) nil))(terpri)
(print (apply '(x x) '(1)))(terpri)
(print (apply '(x x) '(1 2)))(terpri)
(print (apply '(x x) 1 2 '(3 4)))(terpri)

(message "Smoke-testing EVAL...")
(eval ''(error))

(message "Smoke-testing FUNCALL...")
(funcall print 1)(terpri)
(funcall '((x) (print x)) 1)(terpri)

(message "Smoke-testing EVAL...")
(eval '(print 1))(terpri)

(message "Testing ?...")
(? t nil (error))
(? nil (error) nil)
(? nil (error) nil (error))

(message "Testing CAR...")
(? (car nil)
   (error))

(message "Testing CDR...")
(? (cdr nil)
   (error))

(message "Testing NOT...")
(or (not nil) (error))

(message "Testing ATOM...")
(? (atom nil) nil (error))
(? (atom t) nil (error))

(message "Testing CONS?...")
(? (cons? '(1)) nil (error))
(? (cons? 1) (error))

(message "Testing SYMBOL?...")
(? (symbol? 'a) nil (error))

(message "Testing NUMBER?...")
(? (number? 1) nil (error))
(? (number? '(1)) (error))

(message "Testing ==...")
(or (== 1 1) (error))
(and (== 1 0) (error))

(message "Testing =...")
(= x 42)
(or (== x 42) (error))
(= x 23)
(or (== x 23) (error))

(message "Testing VALUE...")
(or (== (value 'x) 23) (error))

; string cons setcar setcdr

(message "Testing >...")
(or (> 42 23) (error))
(message "Testing <...")
(or (< 23 42) (error))
(message "Testing >=...")
(or (>= 42 23) (error))
(or (>= 42 42) (error))
(message "Testing <=...")
(or (<= 23 42) (error))
(or (<= 23 23) (error))

(message "Testing +...")
(or (== (+ 23 42) 65) (error))
(message "Testing -...")
(or (== (- 42 23) 19) (error))
(message "Testing *...")
(or (== (* 23 42) 966) (error))
(message "Testing /...")
(or (== (/ 200 4) 50) (error))
(message "Testing %...")
(or (== (% 200 4) 0) (error))
(message "Testing ++...")
(or (== (++ 23) 24) (error))
(message "Testing --...")
(or (== (-- 23) 22) (error))
(message "Testing BIT-AND...")
(or (== (bit-and 42 23) 2) (error))
(message "Testing BIT-OR...")
(or (== (bit-or 42 23) 63) (error))
(message "Testing BIT-XOR...")
(or (== (bit-xor 42 23) 61) (error))
(message "Testing BIT-NEG...")
(or (== (bit-neg 42) (- 0 43)) (error))
(message "Testing <<...")
(or (== (<< 42 1) 84) (error))
(message "Testing >>...")
(or (== (>> 23 1) 11) (error))
;(message "Testing PEEK...")
;(or (== 1 (print (peek (rawptr (cons nil nil)))))
;    (error))

(message "Testing GC...")
(print (gc))(out '"B free.")(terpri)

; peek poke sys
; read print open err eof in out terpri fresh-line setin setout putback close load
; fn var universe gc exit
; length butlast last member @

(fn make-count (n)
  (? (< 0 n)
     (cons n (make-count (-- n)))))

(message "Testing recursion with MAKE-COUNT...")
(print (make-count 10))(terpri)

; TODO: (undef 'make-count)
; Messes up the heap with the following GC although it
; merely assignes a new copy of the universe list, leaving
; the symbol alone.

; 2024-06-09: 3:40min (10,000), VIC-20/cc65
(fn block-test (c)
  (block nil
    tag
    (= c (-- c))
    (? (== 0 (% c 100))
       (print c))
    (? (not (== c 0))
       (go tag))))

(message "Testing BLOCK...")
(block-test 10)
(terpri)

(message "Not testing UNDEF...")
;(gc)
;(undef 'block-test)
;(gc)

(message "Testing if arguments are restored on function return...")
(= x 42)
(((x)) 23)
(or (== x 42)
    (error "Argument X not restored on return."))
