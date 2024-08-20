(var x nil)

(message "Testing GC...")
(print (gc))(message '" bytes free.")

(message "Testing ATOM...")
(atom 1)

(message "Testing AND...")
(and nil (error))

(message "Testing OR...")
(or t (error))
(or nil t (error))

(message "Testing if arguments are restored on function return...")
(= x 42)
(((x)) 23)
(or (== x 42)
    (error))

(message "Testing if argument symbol values get set before list is done...")
(= x 42)
(((x y)
   (or (== y 47)
       (error))
  )
 23 (+ x 5))

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

(message "Testing READing dotted pair...")
(or (eq 'a (car '(a . 49)))
    (error))
(or (eq 'b (cdr '(10 . b)))
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
(? (symbol? 1) (error))
(? (symbol? (cons t t)) (error))

(message "Testing NUMBER?...")
(? (number? 1) nil (error))
(? (number? '(1)) (error))

(message "Testing BUILTIN?...")
(or (builtin? print) (error))

(message "Testing ==...")
(or (== 1 1) (error))
(and (== 1 0) (error))

(message "Testing =...")
(= x 42)
(or (== x 42) (error))
(= x 23)
(or (== x 23) (error))

(message "Testing SYMBOL-VALUE...")
(or (== (symbol-value 'x) 23)
    (error))

(message "Testing CHAR-AT...")
(and (char-at 'abc 3)
     (error))
(or (== 97 (char-at 'abc 0))
    (error))
(or (== 98 (char-at 'abc 1))
    (error))
(or (== 99 (char-at 'abc 2))
    (error))

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
(message "Testing LENGTH...")
(or (== 0 (length nil))
    (error))
(or (== 1 (length '(a)))
    (error))
(or (== 2 (length '(a b)))
    (error))
(or (== 3 (length '(a b c)))
    (error))

(message "Testing GC...")
(print (gc))(out '" bytes free.")(terpri)

; peek poke sys
; read print open err eof in out terpri fresh-line setin setout putback close load
; fn var universe gc exit

(fn make-count (n)
  (? (< 0 n)
     (cons n (make-count (-- n)))))

(message "Testing recursion with MAKE-COUNT...")
(print (make-count 10))(terpri)

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
(block-test 101)
(terpri)

(message "Removing MAKE-COUNT and BLOCK-TEST...")
(= *universe* (remove 'make-count (remove 'block-test *universe*)))
(print (gc))(message " bytes free.")

(message "Testing SETOUT...")
(setout stdout)

(message "Testing SETIN...")
(setin stdin)
