(fn group2 (x)
  (and x
       (. (. (car x)
             (and (cdr x)
                  (list (cadr x))))
          (group2 (cddr x)))))
