(or (cons? member-if)
    (load "member-if.lisp"))

(fn find-if (f x)
  (car (member-if f x)))

(message "Testing FIND-IF...")
(or (find-if '((x) (eq x 'i))
             '(l i s p))
    (error))
(and (find-if '((x) (eq x 'x))
              '(l i s p))
     (error))
