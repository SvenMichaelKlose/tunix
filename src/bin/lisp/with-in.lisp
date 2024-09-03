(macro with-in (v x . body)
  (let g (symbol)
    $(with ((,g fnin)
            (,v ,x))
       (when ,v
         (setin ,v))
       ,@body
       (when ,v
         (setin ,g)
         (close ,g)))))
