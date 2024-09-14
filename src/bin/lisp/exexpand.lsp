; TODO: Having this in a QUOTE to MAPCAN is messing up the X
; in $.
(fn expex-moved-arg (x)
  (? (cons? x)
     (!= (symbol)
       (cons ! $((= ,! ,x))))
     (cons x nil)))

(fn exexpand (x)
  (? (atom x)
     (list x)
     (!= (@ expex-moved-arg (cdr x))
       (append (mapcan cdr !)
               (list (cons (car x) (carlist !)))))))
