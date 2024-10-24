(macro push (x l)
  ;"Destructively push on stack."
  $(= ,l (. ,x ,l)))
