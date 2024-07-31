(or (cons? member-if)
    (load "member-if.lisp"))

(fn find-if (f x)
  (car (member-if f x)))
