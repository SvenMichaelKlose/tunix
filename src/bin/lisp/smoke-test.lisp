(var x nil)

(print 'gc)(terpri)
(print (gc))(out "B free.")(terpri)

(print 'atom)(terpri)
(atom 1)

(print 'and)(terpri)
(and nil (error))

(print 'or)(terpri)
(or t (error))
(or nil t (error))

(print 'copy-list)(terpri)
(print (copy-list nil))(terpri)
(print (copy-list '(1)))(terpri)
(print (copy-list '(1 2 3)))(terpri)

(print 'apply)(terpri)
(print (apply '(x x) '(1)))(terpri)
(print (apply '(x x) '(1 2)))(terpri)
(print (apply '(x x) 1 2 '(3 4)))(terpri)

(print 'eval)(terpri)
(eval ''(error))

(print 'funcall)(terpri)
(funcall print 1)(terpri)
(funcall '((x) (print x)) 1)(terpri)

(print 'eval)(terpri)
(eval '(print 1))(terpri)

(print '?)(terpri)
(? t nil (error))
(? nil (error) nil)
(? nil (error) nil (error))

(print 'car)(terpri)
(? (car nil)
   (error))

(print 'cdr)(terpri)
(? (cdr nil)
   (error))

(print 'not)(terpri)
(or (not nil) (error))

(print 'atom)(terpri)
(? (atom nil) nil (error))
(? (atom t) nil (error))

(print 'cons?)(terpri)
(? (cons? '(1)) nil (error))
(? (cons? 1) (error))

(print 'symbol?)(terpri)
(? (symbol? 'a) nil (error))

(print 'number?)(terpri)
(? (number? 1) nil (error))
(? (number? '(1)) (error))

(print '==)(terpri)
(or (== 1 1) (error))
(and (== 1 0) (error))

(print '=)(terpri)
(= x 42)
(or (== x 42) (error))
(= x 23)
(or (== x 23) (error))

(print 'value)(terpri)
(or (== (value 'x) 23) (error))

; string cons setcar setcdr

(print '>)(terpri)
(or (> 42 23) (error))
(print '<)(terpri)
(or (< 23 42) (error))
(print '>=)(terpri)
(or (>= 42 23) (error))
(or (>= 42 42) (error))
(print '<=)(terpri)
(or (<= 23 42) (error))
(or (<= 23 23) (error))

(print '+)(terpri)
(or (== (+ 23 42) 65) (error))
(print '-)(terpri)
(or (== (- 42 23) 19) (error))
(print '*)(terpri)
(or (== (* 23 42) 966) (error))
(print '/)(terpri)
(or (== (/ 200 4) 50) (error))
(print '%)(terpri)
(or (== (% 200 4) 0) (error))
(print '++)(terpri)
(or (== (++ 23) 24) (error))
(print '--)(terpri)
(or (== (-- 23) 22) (error))
(print 'bit-and)(terpri)
(or (== (bit-and 42 23) 2) (error))
(print 'bit-or)(terpri)
(or (== (bit-or 42 23) 63) (error))
(print 'bit-xor)(terpri)
(or (== (bit-xor 42 23) 61) (error))
(print 'bit-neg)(terpri)
(or (== (bit-neg 42) (- 0 43)) (error))
(print '<<)(terpri)
(or (== (<< 42 1) 84) (error))
; TODO: False error "Too many args to builtin: 1".
; Should be 84.
;(or (== (print (<< 42 1) 84)) (error))
(print '>>)(terpri)
(or (== (>> 23 1) 11) (error))

(print 'gc)(terpri)
(print (gc))(out "B free.")(terpri)

; peek poke sys
; read print open err eof in out terpri fresh-line setin setout putback close load
; fn var universe gc exit
; length butlast last member @
(terpri)

(fn make-count (n)
  (? (< 0 n)
     (cons n (make-count (-- n)))))

(print 'Recursion)(terpri)
(print (make-count 50))(terpri)
(undef 'make-count)

; 2024-06-09: 3:40min (10,000), VIC-20/cc65
(fn block-test (c)
  (block nil
    tag
    (= c (-- c))
    (? (== 0 (% c 100))
       (print c))
    (? (not (== c 0))
       (go tag))))

(print 'block)(terpri)
(block-test 10)
(undef 'block-test)
(terpri)

(print 'gc-in-block)(terpri)
(block nil
  (print (gc))(out "B free.")(terpri))
