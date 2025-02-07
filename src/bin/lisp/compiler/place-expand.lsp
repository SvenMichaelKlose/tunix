(in-package 'c/pe
  '(scope-place expand-atom expand))

(fn scope-place (fi x)
  (fn expr (fi x)
    (fi.add-free-var x)
    $(%vec ,(fi.scope-arg)
           ,(fi.parent.name)
           ,x))
  (fn find (fi x)
    (? ((fi.parent).arg-or-var? x)
       (expr fi x)
       (find fi.parent x)))
  (fn place (fi x)
    (fi.setup-scope x)
    (!= (find fi x)
      $(%vec ,(expand-atom fi
              (scope-place fi .!.))
             ,..!.
             ,...!.)))
  (? (fi.scope-arg? x)
     x
     (place fi x)))

(fn expand-atom (fi x)
  (?
    (or (literal? x)
        (not (fi.have x)))
      x
    (fi.arg-or-var? x)
      $(%s ,fi.name ,x)
    (fi.scoped-var? x)
      $(%vec ,(expand-atom fi fi.scope)
             ,fi.name
             ,x)
    (fi.global-var? x)
      $(%global ,x)
    (scope-place fi x)))

(def-tree-filter expand (x fi)
  (atom x)
    (expand-atom fi x)
  (or (%closure? x)
      (%stackarg? x))
    x
  (named-lambda? x)
    (copy-lambda x
      :body (expand (lambda-body x)
                    (lambda-funinfo x)))
  (or (%=? x)
      (%set-local-fun? x))
    (!= (expand .x. fi)
       (? (%vec? !)
          $(%=-vec ,.!. ,..!. ,...!.
                   ,(expand ..x. fi))
          x))
  (%slot-value? x)
    $(%slot-value ,(expand .x. fi)
                  ,..x.))

(fn compiler/place-expand (x)
  (expand x (global-funinfo)))

(in-package nil)
