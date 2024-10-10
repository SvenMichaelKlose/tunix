(macro dolist-indexed (iter . body)
  ;"DOLIST plus zero-index."
  (with (i  (car iter)
         c  (cadr iter)
         l  (caddr iter)
         r  (cdddr iter))
    $(with (,c 0)
       (dolist (,i ,l ,@r)
         ,@body
         (!++ ,c)))))
