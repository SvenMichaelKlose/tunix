(macro case x
  (let g (symbol)
    (let f $((p)
              (? (cdr p)
                 $((eql ,g ,',(car p)) ,',(cadr p))
                 (list (car p))))
      $(let ,g ,(car x)
         (? ,@(mapcan f (group2 (cdr x))))))))
