(macro acase x
  (let* (gk  (symbol)
         ga  (symbol)
         f   $((p)
                (? .p
                   $((assoc ,',(car p) ,ga) ,',(cadr p))
                   (.. p.))))
    $(let (,gk ,x.
           ,ga ,.x.)
       (? ,@(mapcan f (group2 ..x))))))
