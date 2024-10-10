(fn group2 (x)
  (and x
       (cons (cons (car x)
                   (and (cdr x)
                        (list (cadr x))))
             (group2 (cddr x)))))
