(in-package 'compiler/place-expand
  '(scope-place stackplace expand-atom
    fun expand))

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
  (? (fi.scope-arg? x)
     x
     (progn
       (fi.setup-scope x)
       (!= (find fi x)
         $(%vec ,(expand-atom fi
                     (scope-place fi .!.))
                ,..!.
                ,...!.)))))

(fn stackplace (fi x)
  `(%stack ,fi.name ,x))

(fn expand-atom (fi x)
  (?
    (or (literal? x)
        (not (fi.has x)))
      x
    (fi.arg-or-var? x)
      (stackplace fi x)
    (fi.scoped-var? x)
      $(%vec ,(expand-atom fi fi.scope)
             ,fi.name
             ,x)
    (fi.global-var? x)
      $(%global ,x)
    (scope-place fi x)))

(fn fun (x)
  (copy-lambda x
      :body (expand (lambda-body x)
                    (lambda-funinfo x))))

(def-tree-filter expand (x fi)
  (atom x)
    (expand-atom fi x)
  (or (%closure? x)
      (%stackarg? x))
    x
  (named-lambda? x)
    (fun x)
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

(fn place-expand (x)
  (expand x (global-funinfo)))

(in-package nil)
