(var *props* nil)
(var *m* nil)

(fn %type? (x)
  (eq t x.))

(fn super (cls x)
  (member-if %type? .*m*))

(fn %cq (x)
  $(. ',x. ,(? .x .x. x)))

(macro class (cls . members)
  (let (name (? (atom cls) cls cls.)
        base (? (cons? cls) .cls.))
    $(push (. (. t ',name)
              (nconc (.. ,@(@ %cq
                              (group2 members)))
                     (%slot-value *props* ,base)))
           *props*)))

(macro method (cls n a . body)
  $(push ',$(,n ,(. 'this a)
              ,@body)
         (slot-value *props* cls)))

(fn new (cls . x)
  (let (o (copy-alist (slot-value *props* cls)))
    (!? o.construct
        (apply ! o x)
        o)))
