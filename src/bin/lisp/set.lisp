(fn unique (x)
  ;"Unique elements of a list."
  (when x
    (? (member (car x) (cdr x))
       (unique (cdr x))
       (cons (car x) (unique (cdr x))))))

(fn adjoin (x lst . args)
  ;"Add an element to a set."
  (? (apply member x lst args)
     lst
     (cons x lst)))

(macro set-op (name . body)
  $(fn ,name (a b)
     ,@body ))

(eval (macroexpand '(set-op intersect
  ;"Elements that are in both lists."
  (and a b
       (? (member (car a) b)
          (cons (car a) (intersect (cdr a) b))
          (intersect (cdr a) b))))))

(eval (macroexpand '(set-op set-difference
  ;"Elements in list b that are not in list a."
  (and b
       (? (member (car b) a)
          (set-difference a (cdr b))
          (cons (car b) (set-difference a (cdr b))))))))

(eval (macroexpand '(set-op union
  ;"Unique elements from both lists."
  (unique (append a b)))))

(eval (macroexpand '(set-op set-exclusive-or
  ;"Elements that are not in both lists."
  (!= (intersect a b)
    (append (remove-if $((x) (member x ,!)) a)
            (remove-if $((x) (member x ,!)) b))))))

(eval (macroexpand '(set-op subseq?
  ;"Check if list a is a subset of list b."
  (every $((x) (member x ,a)) b))))
