(macro do* (vars brk-res . body)
  (let (tag (symbol))
     $((,(@ car vars)
        (block nil
          ,@(@ '((x) $(= ,(car x) ,(cadr x))) vars)
          ,tag
          (? ,brk-res.
             (return ,(? ..brk-res
                         $((()
                             ,',@.brk-res))
                         .brk-res.)))
          ,@body
          ,@(@ '((x)
                  (!? ..x.
                      $(= ,(car x) ,!)))
               vars)
          (go ,tag)))
       ,@(dup nil (length vars)))))
