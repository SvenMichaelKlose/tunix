(macro case x
  (with (g (symbol))
    (with (f $((p)
                (? (cdr p)
                   $((eql ,g ,',(car p)) ,',(cadr p))
                   (list (car p)))))
      $(with (,g ,(car x))
         (? ,@(mapcan f (group2 (cdr x))))))))
