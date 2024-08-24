(macro push (x l)
  ;"Destructively push on stack."
  $(= ,l (cons ,x ,l)))

(macro pop (l)
  ;"Destructively pop from stack."
  $(prog1 (car ,l)
     (= ,l (cdr ,l))))
