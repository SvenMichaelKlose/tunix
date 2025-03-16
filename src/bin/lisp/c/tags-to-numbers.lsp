(in-package 'c/tn
  '(expr tag-map tag-destinations))

(fn tag-destinations (x)
  (@ cadr (remove-if-not %tag? x)))

(fn tag-map (x)
  (let (n 0
        m nil)
    (dolist (e (tag-destinations x) m)
      (acons! x n m)
      (++! n))))

(fn expr (tags x)
  (?
    (%tag? x)
      (slot-value tags .x.)
    (any-%go? x)
      $(,x. ,(slot-value tags .x.))
    x))

(fn c/tags-to-numbers (x)
 (!= (tag-map x)
   (@ $((_)
         (expr ,! _))
      x)))
