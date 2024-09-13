(macro push (x l)
  ;"Destructively push on stack."
  $(= ,l (cons ,x ,l)))
