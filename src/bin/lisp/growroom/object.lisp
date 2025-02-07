(fn slot (obj slot)
  (car (assoc slot obj)))

(fn =slot (val obj slot)
  (=-cdr (assoc slot obj) val))

(fn make-object x
  (@ .. x))

(!= (make-object '(name surname age))
  (= (!.name)    "Klose"
     (!.surname) "Sven"
     (!.age)     "49"))
