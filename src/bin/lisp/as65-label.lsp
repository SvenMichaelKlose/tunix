(var *previous-labels* nil)
(var *next-labels*     nil)
(var *imported-labels* nil)
(var *label-changed?*  nil)

(fn clear-labels ()
  (= *previous-labels* nil)
  (= *next-labels*     nil)
  (= *label-changed?*  nil))

(fn rewind-labels ()
  (= *next-labels* (reverse *previous-labels*))
  (= *previous-labels* nil))

(fn update-label (x addr)
  (!= *next-labels*.
    (unless (eq !. x)
      (error "Wanted to update label ~A but found ~A.~%"
             "Please make sure that your code does not"
             " change across passes."
             x !.))
    (unless (== .! addr)
      (= .! addr)
      (= *label-changed?* t)))
  (push (pop *next-labels*) *previous-labels*))

(fn add-label (x addr)
  (? (first-pass?)
     (acons! x addr *previous-labels*)
     (update-label x addr)))

(fn get-label-in (ltab x direction required?)
  (or (cdr (assoc x ltab :test #'eq))
      (when required?
        (error "No " direction " label " x))))

(fn get-earlier-label (x &key (required? t))
  (get-label-in *previous-labels* x "previous" required?))

(fn get-later-label (x &key (required? t))
  (get-label-in *next-labels* x "next" required?))

(fn has-label? (x)
  (or (cdr (assoc x *previous-labels* :test #'eq))
      (cdr (assoc x *next-labels* :test #'eq))))

(fn get-label-undirected (x &key (required? t))
  (let (prev  (get-earlier-label x :required? nil)
        next  (get-later-label x :required? nil))
    (and required? prev next
       (error "Label " x " appears in earlier and later code.~%"
              "Please specify a direction by prependig a `+` or `-`.~%"
              "If you want to look up the label from inside a"
              " Lisp expression, please see functions GET-EARLIER-LABEL"
              " and GET-LATER-LABEL.~%"))
    (or prev next
        (cdr (assoc x *imported-labels* :test #'eq))
        (and required?
             (error "Label " x " is not defined")))))

(fn get-label (x &key (required? t))
  (when x
    (let (n (symbol-name x))
      (!= (and (< 1 (length n)) 
               (char-at n 1))
        (case (elt n 0)
          #\-  (get-earlier-label ! :required? required?)
          #\+  (get-later-label ! :required? required?)
          #\<  (low (or (get-label ! :required? required?) 0))
          #\>  (high (or (get-label ! :required? required?) 0))
          (get-label-undirected x :required? required?))))))

(fn get-labels ()
  *next-labels*)
