(out "Tests.")(terpri)

; Poke built-ins.
(out "ATOM")(terpri)
(atom 1)
(out "OR")(terpri)
(or nil t)
(or t nil)
(out "APPLY")(terpri)
(print (apply print '(1)))(terpri)
(out "FUNCALL")(terpri)
(funcall print 1)(terpri)
(funcall '((x) (print x)) 1)(terpri)
(out "EVAL")(terpri)
(eval '(print 1))
(out "?")(terpri)
(? t nil (error))
(? nil (error) nil)
(? nil (error) nil (error))
(out "AND")(terpri)
(and nil (error))
(out "OR")(terpri)
(or t (error))
(out "NOT")(terpri)
(or (not nil) (error))
(out "ATOM")(terpri)
(? (atom nil) nil (error))
(? (atom t) nil (error))
(out "CONS?")(terpri)
(? (cons? '(1)) nil (error))
(? (cons? 1) (error))
(out "SYMBOL?")(terpri)
(? (symbol? 'a) nil (error))
(out "NUMBER?")(terpri)
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
