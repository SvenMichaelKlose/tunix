(or (macro? '!?)
    (load "aif.lisp"))

(fn position (x l . f)
  (= f (? f (car f) eql))
  (and l
       (? (f x (car l))
          0
          (!? (position x (cdr l) f)
              (++ !)))))

(message "Testing POSITION...")
(and (position 'a '(l i s p))
     (error))
(or (== 0 (position 'l '(l i s p)))
    (error))
(or (== 2 (position 's '(l i s p)))
    (error))
