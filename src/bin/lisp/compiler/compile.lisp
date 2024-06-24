; Most simple compiler to make bytecode functions
;
; Fast calls of other functions.
; Fast user-fun call.
(fn compile (x)
  (print-file 'cmacros.tmp (compiler-macroexpand x))
  (print-file 'lambda.tmp (lambda-expand (read-file 'cmacros.tmp)))
  (print-file 'expex.tmp (expex (read-file 'lambda.tmp)))
  (print-file 'opt.tmp (optimize (read-file 'expex.tmp)))
  (string (code-expand (read-file 'opt.tmp)) t)
