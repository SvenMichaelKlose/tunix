(macro with-out (v x . body)
  (let g (symbol)
    $(with ((,g fnout)
            (,v ,x))
       (when ,v
         (setout ,v))
       ,@body
       (when ,v
         (setout ,g)
         (close ,g)))))
