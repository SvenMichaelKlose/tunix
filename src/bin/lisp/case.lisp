(or (cons? group)
    (load "group.lisp"))

(fn %case (g x)
  (@ $((p)
        (? (cdr p)
           $((eql ,g ,',(car p)) ,',(cadr p))
           (list (car p))))
     (group (cdr x) 2)))

(macro case x
  (let g (symbol)
    $(let ,g ,(car x)
       (?
         ,@(apply append (%case g x))))))

(message "Testing CASE...")
(or (eq b (case 23
            42 (error)
            23 'b
            (error)))
    (error))
