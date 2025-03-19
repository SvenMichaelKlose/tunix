(or (macro? do)
    (load 'do.lsp))
(or (cons? every)
    (load 'every.lsp))

(fn %dess (x)
  (let (x (split \. x))
    $(cdr (assoc ',(symbol (car (last x)))
                 ,(%des (apply append (pad (.. \.) (butlast x))))))))

(fn %des (x)
  (? (and (not (every '((x)
                         (== \. x))
                      x))
          (member \.  x)
          (not (member \  x)))
     (?
       (eql \. (car (last x)))
         $(car ,(%des (butlast x)))
       (eql \. (car x))
         $(cdr ,(%des (cdr x)))
       (%dess x))
     (symbol x)))

(fn %deqq (x)
  (?
    (and x (symbol? x))
      (%des (symbol-name x))
    (atom x)
      x
    (eq 'unquote (car x))
      (.. 'unquote (dotexpand (cadr x)))
    (. (%deqq (car x))
       (%deqq (cdr x)))))

(fn dotexpand (x)
  (?
    (and x (symbol? x))
      (%des (symbol-name x))
    (or (atom x)
        (eq 'quote (car x)))
      x
    (eq 'backquote (car x))
      (.. 'backquote (%deqq (cadr x)))
    (. (dotexpand (car x))
       (dotexpand (cdr x)))))

(= *ex* '((x) (macroexpand (dotexpand x))))
