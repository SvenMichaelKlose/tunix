(fn %block? (x)
  (and (cons? x)
       (eq '%block (car x))))

(fn fold-block (x)
  ; "Splice rest of %BLOCKs."
  (? (%block? x)
     (!= (cdr x)
       (? (cons? !)
          (mapcan fold-block !)
          (list !)))
     (list x)))
