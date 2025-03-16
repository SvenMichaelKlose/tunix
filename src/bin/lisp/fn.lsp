(fn fn? (x)
  (eq 'fn x.))

(macro fn (args . body)
  "Allow nested FN definitions."
  (eval $(fn ,args
           ,@(!? (remove-if-not #'fn? body)
                 `((let ,(+@ [`(,._. #',.._)] !)
                     ,@(remove-if #'fn? body)))
                 body))))
