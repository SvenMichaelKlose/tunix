(macro with-in (v x . body)
  (let g (symbol)
    $(with ((,g fnin)
            (,v ,x))
       (setin ,v)
       ,@body
       (setin ,g))))
