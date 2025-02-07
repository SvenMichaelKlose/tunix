(macro pop (l)
  ;"Destructively pop from stack."
  $(prog1 (car ,l)
     (= ,l (cdr ,l))))
