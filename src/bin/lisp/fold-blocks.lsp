(fn %block? (x)
  (and (cons? x)
       (eq '%block (car x))))

(fn fold-block (x)
  (? (%block? x)
     (!= (cdr x)
       (? (cons? !)
          (fold-blocks !)
          (list !)))
     (list x)))

(fn fold-blocks (x)
  (mapcan fold-block x))
