(macro do (vars brk-res . body)
  (let (tag (symbol))
     $(let ,(apply append
                   (@ '((x)
                         $(,(car x) ,(cadr x)))
                      vars))
        (block nil
          ,tag
          (? ,(car brk-res)
             (return ,(? (cddr brk-res)
                         $((()
                             ,',@(cdr brk-res)))
                         (cadr brk-res))))
          ,@body
          ,@(@ '((x)
                  (? (caddr x)
                     $(= ,(car x) ,(caddr x))))
               vars)
          (go ,tag)))))
