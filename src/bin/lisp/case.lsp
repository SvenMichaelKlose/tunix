(macro case x
  (let (g (symbol))
    (let (f $((p)
               (? .p
                  $((eql ,g ,',p.) ,',.p.)
                  (list p.))))
      $(let (,g ,x.)
         (? ,@(mapcan f (group2 .x)))))))
