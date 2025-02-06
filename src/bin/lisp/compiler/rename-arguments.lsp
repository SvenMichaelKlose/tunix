(in-package c/ra
  '(argument-sym add-args r))

(def-gensym argument-sym a)

(fn add-args (x replacements)
  (+ (@ [. _ (argument-sym)]
        (argument-expand-names
            (lambda-name x)
            (lambda-args x)))
     replacements))

(define-tree-filter r (x replacements)
  (atom x)
    (or (assoc-value x replacements) x)
  (quote? x)
    x
  (%slot-value? x)
    `(%slot-value ,(r .x. replacements)
                  ,..x.)
  (lambda? x)
    (? (lambda-funinfo x)
       x
       (!= (add-args x replacements)
         (do-lambda x
           :args (r (lambda-args x) !)
           :body (r (lambda-body x) !)))))

(fn compiler/rename-arguments (x)
  (= *argument-sym-counter* 0)
  (r x nil))
