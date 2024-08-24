(load when.lisp)
(load awhile.lisp)
(fn ls ()
  (let chn (opendir)
    (when chn
      (awhile (readdir chn)
      (print !)(terpri))
    (closedir chn))))
