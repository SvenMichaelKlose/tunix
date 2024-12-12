(load 'as65/package.lsp)

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
  (!= (car *next-labels*)
    (unless (eq x (car !))
      (error "Wanted to update label ~A but found ~A.~%"
             "Please make sure that your code does not"
             " change across passes."
             x (car !)))
    (unless (== (cdr !) addr)
      (= (cdr !) addr)
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

(fn get-earlier-label (x . required?)
  (= required? (cdr required?))
  (get-label-in *previous-labels* x "previous" required?))

(fn get-later-label (x . required?)
  (= required? (cdr required?))
  (get-label-in *next-labels* x "next" required?))

(fn has-label? (x)
  (or (cdr (assoc x *previous-labels* :test #'eq))
      (cdr (assoc x *next-labels* :test #'eq))))

(fn get-label-undirected (x . required?)
  (= required? (cdr required?))
  (let (prev  (get-earlier-label x nil)
        next  (get-later-label x nil))
    (when required? prev next
      (error "Label " x " appears in earlier and later code.~%"
             "Please specify a direction by prependig a `+` or `-`.~%"
             "If you want to look up the label from inside a"
             " Lisp expression, please see functions GET-EARLIER-LABEL"
             " and GET-LATER-LABEL.~%"))
    (or prev next
        (cdr (assoc x *imported-labels* :test #'eq))
        (and required?
             (error "Label " x " is not defined")))))

(fn get-label (x . required?)
  (= required? (cdr required?))
  (when x
    (let (n (symbol-name x))
      (!= (and (< 1 (slength x)) 
               (cdr n))
        (case (car r)
          #\-  (get-earlier-label ! required?)
          #\+  (get-later-label ! required?)
          #\<  (bit-and (or (get-label ! required?) 0) 255)
          #\>  (>> (or (get-label ! required?) 0) 8)
          (get-label-undirected x required?))))))

(fn get-labels ()
  *next-labels*)

(in-package nil)
