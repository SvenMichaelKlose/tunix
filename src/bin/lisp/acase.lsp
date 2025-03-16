(macro acase x
  ;"CASE by SLOT-VALUE."
  (let* (gk  (symbol)
         ga  (symbol)
         f   $((p)
                (? .p
                   $((assoc ,',(car p) ,ga) ,',(cadr p))
                   (.. p.))))
    $(let (,gk ,x.
           ,ga ,.x.)
       (? ,@(+@ f (group2 ..x))))))
