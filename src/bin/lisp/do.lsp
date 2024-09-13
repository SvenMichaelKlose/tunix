(macro do (vars brk-res . body)
  (let tag (symbol)
     $(with ,(@ '((x)
                   $(,(car x) ,(cadr x)))
                vars)
        (block nil
          ,tag
          (? ,(car brk-res)
             (return (progn ,@(cdr brk-res))))
          ,@body
          ,@(@ '((x)
                  (? (caddr x)
                     $(= ,(car x) ,(caddr x))))
               vars)
          (go ,tag)))))
