(var *=* nil)

(macro =fn (name args . body)
  (!= (symbol (. \= (symbol-name name)))
    (push *=* (. name !))
    $(%fn ,! ,args ,@body)))

(macro = x
  (? (atom x.)
     x
     (!? (assoc (car x.) *=*)
         $(,.! ,.x. ,(cdr x.))
         (error x. "Not settable"))))
