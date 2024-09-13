(var x nil)

(and (builtin? gc)
  ((()
     (message "Testing GC...")
     (print (gc))(out '" bytes free.")(terpri))))

(message "Testing ATOM...")
(atom 1)

(message "Testing AND...")
(and nil (error))

(message "Testing OR...")
(or t (error))
(or nil t (error))

(message "Testing if argument symbol values are restored on return...")
(= x 42)
(((x)) 23)
(or (== x 42)
    (error))

(message "Testing if argument values get set after evaluation has completed...")
(= x 42)
(((x y)
   (or (== y 47)
       (error))
  )
 23 (+ x 5))

(message "Missing rest arguments...")
((rest))

(message "Following, missing rest arguments...")
(((first . rest)) t)

(message "Smoke-testing COPY-LIST...")
(copy-list nil)
(copy-list '(1))
(copy-list '(1 2 3))

(message "Smoke-testing APPLY...")
(apply '(x x) nil)
(apply '(x x) '(1))
(apply '(x x) '(1 2))
(apply '(x x) 1 2 '(3 4))

(message "Smoke-testing EVAL...")
(eval ''(error))

(message "Smoke-testing FUNCALL...")
(funcall print 1)(terpri)
(funcall '((x) (print x)) 1)(terpri)

(message "Smoke-testing EVAL...")
(eval '(print 1))(terpri)

(message "Smoke-testing SYMBOL-NAME...")
(symbol-name nil)
(symbol-name t)

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

(message "Testing to READ dotted pair...")
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

(message "Testing LENGTH...")
(or (== 0 (length nil))
    (error))
(or (== 1 (length '(a)))
    (error))
(or (== 2 (length '(a b)))
    (error))
(or (== 3 (length '(a b c)))
    (error))

(message "Testing ==...")
(or (== 1 1) (error))
(and (== 1 0) (error))

(message "Testing =...")
(= x 42)
(or (== x 42) (error))
(= x 23)
(or (== x 23) (error))

(message "Testing uniqueness of anonymous symbols...")
(and (eq (symbol) (symbol))
     (error))
(and (eq "" (symbol))
     (error))
(and (eq "" "")
     (error))

(message "Testing SYMBOL-VALUE...")
(or (== (symbol-value 'x) 23)
    (error))

(and (builtin? char-at)
  ((()
     (message "Testing CHAR-AT...")
     (and (char-at 'abc 3)
          (error))
     (or (== 97 (char-at 'abc 0))
         (error))
     (or (== 98 (char-at 'abc 1))
         (error))
     (or (== 99 (char-at 'abc 2))
         (error)))))

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

(and (builtin? bit-and)
  ((()
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
     (or (== (>> 23 1) 11) (error)))))
;(message "Testing PEEK...")
;(or (== 1 (print (peek (rawptr (cons nil nil)))))
;    (error))

(and (builtin? gc)
  ((()
     (message "Testing GC...")
     (print (gc))(out '" bytes free.")(terpri))))

; peek poke sys
; read print open err eof in out terpri fresh-line setin setout putback close load
; fn var universe gc exit

(fn make-count (n)
  (? (< 0 n)
     (cons n (make-count (-- n)))))

(message "Smoke-testing recursion...")
(make-count 10)

(message "Testing RETURN...")
((()
  (block nil
    ((()
      ((()
        (and (or (return nil)))))
      (error "RETURN should have been returned from parent function body.")))
    (error "Should have returned past BLOCK."))))

(fn return-test (block-name)
  (((x)
    (block b1
      (block b2
        (block b3
          (return nil block-name))
        (= x 'b2e))
      (= x 'b1e))
    x) 'b0))

(= *b* (cons 'return *b*))
(or (eq 'b1e (return-test 'b3))
    (error))
(or (eq 'b1e (return-test 'b2))
    (error))
(or (eq 'b0 (return-test 'b1))
    (error))

; 2024-06-09: 3:40min (10,000), VIC-20/cc65
(message "Testing BLOCK...")
(fn block-test (c)
  (block nil
    tag
    (= c (-- c))
    (? (== 0 (% c 100))
       (print c))
    (? (not (== c 0))
       (go tag))))

(and (block-test 101)
     (error))

(message "Smoke-testing removal from *UNIVERSE*...")
(= *universe* (remove 'make-count *universe*))
(= *universe* (remove 'block-test *universe*))
(= *universe* (remove 'return-test *universe*))
(and (builtin? gc)
  ((()
     (print (gc))(out " bytes free.")(terpri))))

(message "Testing SETIN...")
(setin stdin)

(message "Testing SETOUT...")
(setout stdout)
