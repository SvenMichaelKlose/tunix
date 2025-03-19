(require 'dotexpand 'mkpkg)

(var *p* nil)

(fn in-package (pkg . syms)
  (= *p* (. pkg syms.)))

(= *ex* '((x) (macroexpand (mkpkg (dotexpand x)))))
