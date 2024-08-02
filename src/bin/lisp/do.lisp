; (do ((var init step)*)
;     (break-condition result)
;   body*)
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

(message "Testing DO...")
(do ((i 0 (+ i 1)))
    ((>= i 10))
  (print i))
