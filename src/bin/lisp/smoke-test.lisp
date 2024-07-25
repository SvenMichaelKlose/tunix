(var x nil)

(out "Testing GC...")(terpri)
(print (gc))(out '"B free.")(terpri)

(out "Testing ATOM...")(terpri)
(atom 1)

(out "Testing AND...")(terpri)
(and nil (error))

(out "Testing OR...")(terpri)
(or t (error))
(or nil t (error))

(out "Smoke-testing COPY-LIST...")(terpri)
(print (copy-list nil))(terpri)
(print (copy-list '(1)))(terpri)
(print (copy-list '(1 2 3)))(terpri)

(out "Smoke-testing APPLY...")(terpri)
(print (apply '(x x) nil))(terpri)
(print (apply '(x x) '(1)))(terpri)
(print (apply '(x x) '(1 2)))(terpri)
(print (apply '(x x) 1 2 '(3 4)))(terpri)

(out "Smoke-testing EVAL...")(terpri)
(eval ''(error))

(out "Smoke-testing FUNCALL...")(terpri)
(funcall print 1)(terpri)
(funcall '((x) (print x)) 1)(terpri)

(out "Smoke-testing EVAL...")(terpri)
(eval '(print 1))(terpri)

(out "Testing ?...")(terpri)
(? t nil (error))
(? nil (error) nil)
(? nil (error) nil (error))

(out "Testing CAR...")(terpri)
(? (car nil)
   (error))

(out "Testing CDR...")(terpri)
(? (cdr nil)
   (error))

(out "Testing NOT...")(terpri)
(or (not nil) (error))

(out "Testing ATOM...")(terpri)
(? (atom nil) nil (error))
(? (atom t) nil (error))

(out "Testing CONS?...")(terpri)
(? (cons? '(1)) nil (error))
(? (cons? 1) (error))

(out "Testing SYMBOL?...")(terpri)
(? (symbol? 'a) nil (error))

(out "Testing NUMBER?...")(terpri)
(? (number? 1) nil (error))
(? (number? '(1)) (error))

(out "Testing ==...")(terpri)
(or (== 1 1) (error))
(and (== 1 0) (error))

(out "Testing =...")(terpri)
(= x 42)
(or (== x 42) (error))
(= x 23)
(or (== x 23) (error))

(out "Testing VALUE...")(terpri)
(or (== (value 'x) 23) (error))

; string cons setcar setcdr

(out "Testing >...")(terpri)
(or (> 42 23) (error))
(out "Testing <...")(terpri)
(or (< 23 42) (error))
(out "Testing >=...")(terpri)
(or (>= 42 23) (error))
(or (>= 42 42) (error))
(out "Testing <=...")(terpri)
(or (<= 23 42) (error))
(or (<= 23 23) (error))

(out "Testing +...")(terpri)
(or (== (+ 23 42) 65) (error))
(out "Testing -...")(terpri)
(or (== (- 42 23) 19) (error))
(out "Testing *...")(terpri)
(or (== (* 23 42) 966) (error))
(out "Testing /...")(terpri)
(or (== (/ 200 4) 50) (error))
(out "Testing %...")(terpri)
(or (== (% 200 4) 0) (error))
(out "Testing ++...")(terpri)
(or (== (++ 23) 24) (error))
(out "Testing --...")(terpri)
(or (== (-- 23) 22) (error))
(out "Testing BIT-AND...")(terpri)
(or (== (bit-and 42 23) 2) (error))
(out "Testing BIT-OR...")(terpri)
(or (== (bit-or 42 23) 63) (error))
(out "Testing BIT-XOR...")(terpri)
(or (== (bit-xor 42 23) 61) (error))
(out "Testing BIT-NEG...")(terpri)
(or (== (bit-neg 42) (- 0 43)) (error))
(out "Testing <<...")(terpri)
(or (== (<< 42 1) 84) (error))
(out "Testing >>...")(terpri)
(or (== (>> 23 1) 11) (error))
;(print 'peek)(terpri)
;(or (== 1 (print (peek (rawptr (cons nil nil)))))
;    (error))

(out "Testing GC...")(terpri)
(print (gc))(out '"B free.")(terpri)

; peek poke sys
; read print open err eof in out terpri fresh-line setin setout putback close load
; fn var universe gc exit
; length butlast last member @
(terpri)

(fn make-count (n)
  (? (< 0 n)
     (cons n (make-count (-- n)))))

(out "Testing recursion with MAKE-COUNT...")(terpri)
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

(out "Testing BLOCK...")(terpri)
(block-test 10)
(terpri)

(out "Not testing UNDEF...")(terpri)
;(gc)
;(undef 'block-test)
;(gc)

(out "Testing if arguments are restored on function return...")(terpri)
(= x 42)
(((x)) 23)
(or (== x 42)
    (error "Argument X not restored on return."))
