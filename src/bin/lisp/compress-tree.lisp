(load "when.lisp")
(load "unless.lisp")
(load "dolist.lisp")
(load "with-prof.lisp")

; Replace duplicate subtrees by original.
(fn replace-duplicates (original x)
  (and (cons? original)
       (cons? x)
    (do ((i x (cdr i)))
        ((atom i))
      (unless (eq original (car i))
        (? (equal original (car i))
           (progn
             (out "Found CAR: ")(print original)(terpri)
             (setcar i original))
           (replace-duplicates original (car i))))
      (when (and (not (eq original (cdr i)))
                 (equal original (cdr i)))
        (out "Found CDR: ")(print original)(terpri)
        (setcdr i original)
        (return nil)))))

(fn compress-tree (x path)
  (when (cons? x)
    ; Walk over list
    (do ((i x (cdr i)))
        ((atom i))
      (replace-duplicates (car i) i)
      ; Search in parent CDRs.
      (dolist (j path)
        (replace-duplicates i (cdr j)))
      ; Step into subtree.
      (?
        (cons? (car i))
          (compress-tree (car i) (cons i path))
        (and (not path)
             (symbol? (car i)))
          (progn
            (print (car i))(terpri)
          (compress-tree (symbol-value (car i)) (cons i path)))))))

(compress-tree *universe* nil)
