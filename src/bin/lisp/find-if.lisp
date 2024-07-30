(or (cons? member-if)
    (load "member-if.lisp"))

(fn find-if (pred x)
  (car (member-if pred x)))
