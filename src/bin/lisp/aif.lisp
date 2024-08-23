(macro !? x
  ;"Like '?' but assigning results of conditions to '!'.
  (let rec '((x)
              (and x
                   (? (cdr x)
                      $((= ! ,(car x))
                        ,(cadr x)
                        ,@(rec (cddr x)))
                      (list (car x)))))
    $(let ! nil
       (? ,@(rec x)))))

(message "Testing !?...")
(!? 49
    (or (eql ! 49)
        (error)))
(!?
  nil (error)
  49  (or (eql ! 49)
          (error)))
(or (eql 5 (!? nil
               (error)
               5))
    (error))
