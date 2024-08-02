(fn find (x l)
  (car (member x l)))

(message "Testing FIND...")
(or (find 'i '(l i s p))
    (error))
(and (find 'x '(l i s p))
     (error))
