(fn mapcar (f . l)
  (and (car l)
       (cons (apply f (carlist l))
             (apply mapcar f (cdrlist l)))))

(message "Testing MAPCAR...")
(or (equal (mapcar + '(1 2 3) '(4 5 6))
           '(5 7 9))
    (error))
