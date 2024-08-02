(fn adjoin (x l . args)
  ;"Add an element to a set."
  (? (apply member x l args)
     l
     (cons x l)))

(message "Testing ADJOIN...")
(or (equal (adjoin 'l '(i s p))
           '(l i s p))
    (error))
