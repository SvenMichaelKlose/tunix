(fn mapcar (f . l)
  (and (car l)
       (cons (apply f (mapcar car l))
             (apply mapcar f (mapcar cdr lists)))))

(message "Testing MAPCAR...")
(or (equal (mapcar #'+ '(1 2 3) '(4 5 6) '(7 8 9))
           '(12 15 18))
    (error)
