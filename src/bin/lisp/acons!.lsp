(macro acons! (a d x)
  $(= ,x (. (. ,a ,d) ,x)))
