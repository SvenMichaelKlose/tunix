; TUNIX-Lisp

(print stdin)
(terpri)
(print stdout)
(terpri)
(print 1)
(+ 1 1)
(- 1 1)
(* 23 42)
(/ 23 5)
(% 23 5)
(++ 2)
(-- 2)
(quote x)
'x
(fn myfun (x)
  (print x))
(fn fnord (x))
myfun
(? t 1)
(? t 1 2)
(? nil 1 2)
(? nil 1 t 2)
myfun
(myfun 128)
(var some-list (quote (1 2 3 4)))
(fn length (x)
  (? (cons? x)
     (+ 1 (length (cdr x)))
     0))
(length some-list)
(eval (quote (+ 2 3)))
(& nil nil)
(& nil t)
(& t nil)
(& t t)
(| nil nil)
(| nil t)
(| t nil)
(| t t)
(block nil
  (print 1)
  (go jmp)
  (print 2)
  jmp
  (print 3))
(print 65)
(out 65)
(out "Hello world!")(terpri)
(print (string '(65 66 67)))(terpri)

(var c 10000)

(fn block-test ()
  (out "Looping...")(terpri)
  (block nil
    tag
    (setq c (-- c))
    (? (== 0 (% c 100))
       (print c))
    (? (not (== c 0))
       (go 'tag))))

(block-test)

(gc)
