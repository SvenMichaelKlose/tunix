(var *p* nil)

(fn in-package (pkg . syms)
  (= *p* (. pkg (car syms))))

(fn %mkpkg (x)
  (?
    (not x) nil
    (symbol? x)
      (?
        ; Escaped symbol, prefixed with "/".
        (and (< 1 (slength x))
             (== \/ (char-at x 0)))
          ; Pass through if not a package symbol.
          (!= (symbol (cdr (symbol-name x)))
            (? (member ! (cdr *p*)) ! x))
        ; Prefix with package name if in list.
        (member x (cdr *p*))
          (symbol (append (symbol-name (car *p*))
                          (list \/)
                          (symbol-name x)))
        x)
    (atom x) x
    (. (%mkpkg (car x))
       (%mkpkg (cdr x)))))

(= *ex* '((x) (macroexpand (%mkpkg (dotexpand x)))))
