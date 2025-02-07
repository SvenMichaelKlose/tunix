(fn mapcan (f . l)
  (apply nconc (apply mapcar f l)))
