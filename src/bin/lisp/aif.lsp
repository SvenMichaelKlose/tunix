(macro !? x
  ;"Like '?' but assigning results of conditions to '!'.
  (with (rec '((x)
                (and x
                     (? (cdr x)
                        $((= ! ,(car x))
                          ,(cadr x)
                          ,@(rec (cddr x)))
                        (list (car x))))))
    $(with (! nil)
       (? ,@(rec x)))))
