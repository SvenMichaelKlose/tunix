(fn mapcan (f . l)
  (apply append (apply mapcar f l)))
