(load 'as65/package.lsp)

(fn pass (o addr pathnames first-pass?)
  ; "Single-pass assembly to stream."
  (= *as65-pc* addr)
  (dolist (pn pathnames)
    (with-in i (open pn 'r)
      (awhile (as65-asm (read) first-pass?)
        (when o
          (out !))))))

(fn files (bin-path addr pathnames)
  ; "Assemble a set of files in multiple passes."

  ; First pass, building label database.
  (= *as65-labels* nil)
  (pass nil addr pathnames t)

  ; Secondary passes, until code size settles.
  (let (old-end *as65-pc*)
    (while t
      (pass nil addr pathnames nil)
      (when (== old-end *as65-pc*)
        (return))
      (= old-end *as65-pc*)))

  ; Final pass, outputting the binary.
  (with-out o (open bin-path 'w)
    (pass o addr pathnames nil)))

(in-package nil)
