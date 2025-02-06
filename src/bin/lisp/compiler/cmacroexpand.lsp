(require 'let 'prog1 'push 'pop '!=
         'aif 'with-global 'with-in
         'with-out 'when 'awhen 'acons!
         'while 'group 'mapcar '+@
         'umacro)

(load 'compiler/blockexpand.lsp)
(load 'compiler/controlflow.lsp)

(fn compiler/macroexpand (x)
  (umacroexpand 'compiler x))
