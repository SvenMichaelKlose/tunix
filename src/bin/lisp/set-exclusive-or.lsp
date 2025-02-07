(fn set-exclusive-or (a b)
  ;"Elements that are not in both lists."
  (let (subset  (intersect a b)
        pred    $((x)
                   (member x ,subset)))
    (nconc (remove-if pred a)
           (remove-if pred b))))
