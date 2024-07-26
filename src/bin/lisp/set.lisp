(fn unique (x)
  ;"Unique elements of a list."
  (when x
    (? (member (car x) (cdr x))
       (unique (cdr x))
       (cons (car x) (unique (cdr x))))))

(fn adjoin (x l . args)
  ;"Add an element to a set."
  (? (apply member x l args)
     l
     (cons x l)))

(macro set-op (name . body)
  $(fn ,name (a b)
     ,@body ))

(set-op intersect
  ;"Elements that are in both lists."
  (and a b
       (? (member (car a) b)
          (cons (car a) (intersect (cdr a) b))
          (intersect (cdr a) b))))

(set-op set-difference
  ;"Elements in list b that are not in list a."
  (and b
       (? (member (car b) a)
          (set-difference a (cdr b))
          (cons (car b) (set-difference a (cdr b))))))

(set-op union
  ;"Unique elements from both lists."
  (unique (append a b)))

(set-op set-exclusive-or
  ;"Elements that are not in both lists."
  (with ((subset  (intersect a b))
         (pred    $((x)
                    (member x ,subset))))
    (append (remove-if pred a)
            (remove-if pred b))))

(set-op subseq?
  ;"Check if list a is a subset of list b."
  (every $((x) (member x ,a)) b))
