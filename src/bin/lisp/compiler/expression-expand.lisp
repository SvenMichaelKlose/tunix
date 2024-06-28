(var rv (symbol))

(fn exparg (x)
  (? (atom x)
     (. x (list x))
     (!= (symbol)
       (. ! (expex x !)))))

(fn expex (x v)
  ;"Make single statement assignments of expression."
  (? (atom x)
     $((= ,(or v rv) ,x))
     (!= (@ exparg .x)
       (+ (cdrlist !)
          $((= ,(or v rv) (,x. ,@(carlist !))))))))
