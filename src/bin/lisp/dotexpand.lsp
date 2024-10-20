; Expand dot-notation

(fn %des (x)
  (? (and (member \. x)
          (not (member \  x)))
     (?
       (eql \. (car (last x)))
         $(car ,(%des (butlast x)))
       (eql \. (car x))
         $(cdr ,(%des (cdr x)))
       (!= (split \. x)
         $(cdr (assoc ',(symbol (car (last !)))
                      ,(%des (apply nconc (pad (list \.)
                                               (butlast !))))))))
     (symbol x)))

(fn %debq (x)
  (?
    (and x (symbol? x))
      (%des (symbol-name x))
    (atom x)
      x
    (eq 'unquote (car x))
      (list 'unquote (dotexpand (cadr x)))
    (cons (%debq (car x))
          (%debq (cdr x)))))

(fn dotexpand (x)
  (?
    (and x (symbol? x))
      (%des (symbol-name x))
    (or (atom x)
        (eq 'quote (car x)))
      x
    (eq 'backquote (car x))
      (list 'backquote (%debq (cadr x)))
    (cons (dotexpand (car x))
          (dotexpand (cdr x)))))

(message '"Testing dotexpand...")
(print (dotexpand '.x))
(print (dotexpand 'x.))
(print (dotexpand '.x.))
(print (dotexpand 'x.y))
(print (dotexpand 'x.y.z))
