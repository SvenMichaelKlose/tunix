(require 'let 'prog1 'push 'pop '!=
         'aif 'with-global 'with-in
         'with-out 'when 'awhen 'acons!
         'while 'group 'mapcar '+@
         'umacro)

(load 'compiler/blockexpand.lsp)

(umacro compiler block x
  (c/be/cblock x))

(load 'compiler/controlflow.lsp)

(fn compiler/cmacroexpand (x)
  (umacroexpand 'compiler x))
