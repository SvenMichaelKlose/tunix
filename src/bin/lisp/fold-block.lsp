(or (cons? mapcar)
    (load "mapcar.lsp"))

(app 'app-fold-block)

(fn %block? (x)
  (and (cons? x)
       (eq '%block (car x))))

(fn fold-block (x)
  (? (%block? x)
     (!= (cdr x)
       (? (cons? !)
          (mapcan fold-block !)
          (list !)))
     (list x)))

(app 'app-fold-block)
