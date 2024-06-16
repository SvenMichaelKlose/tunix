(macro push (l v)
  ;"Destructively push on stack."
  $(= ,l (cons ,v ,l)))

(macro pop (l)
  ;"Destructively pop from stack."
  $(prog1 ,(car l)
     (= ,l (cdr ,l))))
