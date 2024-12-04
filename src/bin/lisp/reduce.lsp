(fn reduce (f l . init)
  ;"Reduce a list using function F, optionally starting with INITIAL-VALUE."
  (? l
     (!= (!? init. (funcall f ! l.) l.)
       (@ (i .l !)
         (= ! (funcall f ! i))))
     init))
