(macro with-out (v x . body)
  (let g (symbol)
    $(with ((,g fnout)
            (,v ,x))
       (setout ,v)
       ,@body
       (setout ,g))))
