(fn nthcdr (n l)
  (? (or (not l)
         (== 0 n))
     l
     (nthcdr (-- n) (cdr l))))

(message "Testing NTHCDR...")
(or (equal (nthcdr 2 '(l i s p))
           '(s p))
    (error))
