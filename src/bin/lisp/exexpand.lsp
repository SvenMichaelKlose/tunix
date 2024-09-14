; TODO: Having this in a QUOTE to MAPCAN is messing up the X
; in $.
(fn expex-moved-arg (x)
  (? (cons? x)
     (!= (symbol)
       (cons ! $((= ,! ,x))))
     (cons x nil)))

(fn exexpand0 (x)
  (? (atom x)
     (list x)
     (!= (@ expex-moved-arg (cdr x))
       (append (mapcan cdr !)
               (list (cons (car x) (carlist !)))))))

(fn exexpand (x)
  (mapcan exexpand0 x))
