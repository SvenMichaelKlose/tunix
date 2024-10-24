(var *props* nil)

(fn %cq (x)
  (!= (ensure-list x))
    $(. ',!. ,(? .! .!. !)))

(macro class (base n . x)
  $(push (. (. t ',class)
            (nconc (list ,@(@ %cq (group2 x)))
                   (cdr (assoc ',base *props*))))
         *props*))

(macro method (class n a . body)
  $(push ',$(,n ,(. 'this a) ,@body)
         (assoc class *props*)))

(fn new (class . x)
  (let (o (copy-alist (cdr (assoc class *props*))))
    (!? (cdr (assoc 'constructor o))
        (apply ! o x)
        o)))

(fn slot-value (x s)
  (cdr (assoc s x)))

(fn set-slot-value (x s v)
  (setcdr (assoc s x) v))

(fn %type? (x)
  (eq t x.))

(fn super (x)
  (member-if %type? .*m*))
