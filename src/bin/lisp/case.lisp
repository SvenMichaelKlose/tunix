(or (cons? group)
    (load "group.lisp"))

(macro case x
  (let g (symbol)
    (let f $((p)
              (? (cdr p)
                 $((eql ,g ,',(car p)) ,',(cadr p))
                 (list (car p))))
      $(let ,g ,(car x)
         (? ,@(mapcan f (group (cdr x) 2)))))))

(message "Testing CASE...")
(case 23
  42 (error)
  23 'b
  (error))
(case t
  42 (error)
  65 (error)
  'ok)
