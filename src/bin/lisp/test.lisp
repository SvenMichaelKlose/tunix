(out "Tests.")(terpri)

; Poke built-ins.
(print 'atom)(terpri)
(atom 1)

(print 'and)(terpri)
(and nil (error))

(print 'or)(terpri)
(or nil t)
(or t nil)
(or t (error))

(print 'copy-list)(terpri)
(print (copy-list nil))
(print (copy-list '(1)))
(print (copy-list '(1 2 3)))

(print 'apply)(terpri)
(print (apply '(x x) '(1)))(terpri)
(print (apply '(x x) '(1 2)))(terpri)
(print (apply '(x x) 1 2 '(3 4)))(terpri)

(print 'funcall)(terpri)
(funcall print 1)(terpri)
(funcall '((x) (print x)) 1)(terpri)

(print 'eval)(terpri)
(eval '(print 1))

(print '?)(terpri)
(? t nil (error))
(? nil (error) nil)
(? nil (error) nil (error))

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

; = value string cons car cdr setcar setcdr
; == > < >= <=
; + - * / % ++ --
; bit-and bit-or bit-xor bit-neg << >>
; peek poke sys
; read print open err eof in out terpri fresh-line setin setout putback close load
; fn var universe gc exit
; length butlast last member @
(terpri)

(fn make-count (n)
  (? (< 0 n)
     (cons n (make-count (-- n)))))

; Not really a test.
(print (make-count 100))

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

(fresh-line)
(block-test 300)
