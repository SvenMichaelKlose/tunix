(fn getf (k l . test)
  (? l
     (? (cons? l.)
        (cdr (assoc k l))
        (do ((l l .l))
            ((not l))
          (? (eq k l.)
             (return .l.))))))
