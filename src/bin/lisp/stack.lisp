(or (macro? 'prog1)
    (load "prog1.lisp"))

(macro push (x l)
  ;"Destructively push on stack."
  $(= ,l (cons ,x ,l)))

(macro pop (l)
  ;"Destructively pop from stack."
  $(prog1 (car ,l)
     (= ,l (cdr ,l))))

(message "Testing PUSH and POP...")
(or (let x nil
      (push p x)
      (push s x)
      (push i x)
      (push l x)
      (and (equal x '(l i s p))
           (eq 'l (pop x))
           (equal x '(i s p))))
    (error))
