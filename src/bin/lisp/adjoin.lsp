(fn adjoin (x l . args)
  ;"Add an element to a set."
  (? (apply member x l args)
     l
     (. x l)))
