(macro case x
  (let* (g  (symbol)
         f  $((p)
               (? .p
                  $((eql ,g ,',(car p)) ,',(cadr p))
                  (list p.))))
    $(let (,g ,x.)
       (? ,@(mapcan f (group2 .x))))))
