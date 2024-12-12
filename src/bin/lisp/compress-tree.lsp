; Replace duplicate subtrees by original.
(fn replace-duplicates (original x universe-list?)
  (and (cons? original)
       (cons? x)
    (do ((i x .i))
        ((atom i))
      (unless (eq original i.)
        (?
          (equal original i.)
          (progn
            (out "Found CAR: ")(print original)
            (setcar i original))
          (replace-duplicates original
                              (? (and universe-list?
                                      (symbol? i.))
                                 (symbol-value i.)
                                 i.)
                              nil)))
      (when (and (not (eq original .i))
                 (equal original .i))
        (out "Found CDR: ")(print original)
        (setcdr i original)))))

(fn compress-tree (x . path)
  (= path (and path path.))
  (when (cons? x)
    (unless path
      (? (symbol? x.)
         (print (symbol-value x.))))
    ; Walk over list
    (do ((i x .i))
        ((atom i))
      (replace-duplicates i. i (not path))
      ; Search in parent CDRs.
      (do ((j path .j))
          ((atom j))
        (replace-duplicates i. j.
                            (not .j)))
      ; Step into subtree.
      (?
        (cons? i.)
          (compress-tree i. (. i path))
        (and (not path)
             (symbol? i.))
          (progn
            (print i.)(terpri)
            (compress-tree (symbol-value i.) (. i path)))))))
