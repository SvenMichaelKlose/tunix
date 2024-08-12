(macro !? (c . x)
  ;"Like '?' but assigning result of condition to '!'.
  (? x
     $(let ! ,c
        (? !
           ,(car x)
           ,@(? (cdr x)
                $((%!? ,@(cdr x))))))
     c))
