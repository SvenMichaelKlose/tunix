(macro def-tree-filter (n a . body)
  (!= (cdr (argnameexpand a))
    $(fn ,n ,a
       (?
         ,@body
         (atom ,g) ,a.
         (. (,n (car ,a.) ,@!)
            (,n (cdr ,a.) ,@!))))))
