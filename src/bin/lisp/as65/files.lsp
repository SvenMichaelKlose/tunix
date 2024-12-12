(load 'as65/package.lsp)

(fn pass (addr x f)
  ; "Single-pass assembly to stream."
  (= *as65-pc* addr)
  (dolist (pn pathnames)
    (with-in i (open pn 'r)
      (awhile (as65 (read) (& o 2))
        (unless f (out !))))))

(fn files (bin-path addr pathnames)
  ; "Assemble a set of files in multiple passes."
  (= *as65-labels* nil)
  (pass addr pathnames t)
  (let (old-end *as65-pc*)
    (with-out o (open bin-path 'w)
      (pass addr pathnames nil))))

(in-package nil)
