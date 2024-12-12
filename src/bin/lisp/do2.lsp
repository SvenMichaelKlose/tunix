(macro do* (vars brk-res . body)
  (let (tag (symbol))
     $((,(@ car vars)
        (block nil
          ,@(@ '((x) $(= ,x. ,.x.)) vars)
          ,tag
          (? ,brk-res.
             (return ,(? ..brk-res
                         $((()
                             ,',@.brk-res))
                         .brk-res.)))
          ,@body
          ,@(@ '((x)
                  (!? ..x.
                      $(= ,x. ,!)))
               vars)
          (go ,tag)))
       ,@(dup nil (length vars)))))
