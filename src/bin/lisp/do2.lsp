(macro do* (vars brk-res . body)
  (let (tag (symbol))
     $((,(@ car vars)
        (block nil
          ,@(@ '((x) $(= ,(car x) ,(cadr x))) vars)
          ,tag
          (? ,(car brk-res)
             (return ,(? (cddr brk-res)
                         $(progn
                            ,',@(cdr brk-res))
                         (cadr brk-res))))
          ,@body
          ,@(@ '((x)
                  (!? (caddr x)
                      $(= ,(car x) ,!)))
               vars)
          (go ,tag)))
       ,@(dup nil (length vars)))))
