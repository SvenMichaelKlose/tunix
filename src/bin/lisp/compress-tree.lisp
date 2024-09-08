(load "when.lisp")
(load "unless.lisp")
(load "dolist.lisp")
(load "with-prof.lisp")

; Replace duplicate subtrees by original.
(fn replace-duplicates (original x universe-list?)
  (and (cons? original)
       (cons? x)
    (do ((i x (cdr i)))
        ((atom i))
      (unless (eq original (car i))
        (?
          (equal original (car i))
          (progn
            (out "Found CAR: ")(print original)(terpri)
            (setcar i original))
          (replace-duplicates original
                              (? (and universe-list?
                                      (symbol? (car i)))
                                 (symbol-value (car i))
                                 (car i))
                              nil)))
      (when (and (not (eq original (cdr i)))
                 (equal original (cdr i)))
        (out "Found CDR: ")(print original)(terpri)
        (setcdr i original)))))

(fn compress-tree (x . path)
  (= path (and path (car path)))
  (when (cons? x)
    (unless path
      (? (symbol? (car x))
         (print (symbol-value (car x)))))
    ; Walk over list
    (do ((i x (cdr i)))
        ((atom i))
      (replace-duplicates (car i) i (not path))
      ; Search in parent CDRs.
      (do ((j path (cdr j)))
          ((atom j))
        (replace-duplicates (car i) (car j)
                            (not (cdr j))))
      ; Step into subtree.
      (?
        (cons? (car i))
          (compress-tree (car i) (cons i path))
        (and (not path)
             (symbol? (car i)))
          (progn
            (print (car i))(terpri)
            (compress-tree (symbol-value (car i)) (cons i path)))))))
