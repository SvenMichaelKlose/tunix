(print (gc))(out " bytes heap.")(terpri)
(var *start-time* (time))

(fn message x
  (fresh-line)
  (apply out x)
  (terpri))
