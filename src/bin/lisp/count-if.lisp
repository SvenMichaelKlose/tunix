(fn count-if (f l)
  (let n 0
    (dolist (i l n)
      (? (f i)
         (!++ n)))))