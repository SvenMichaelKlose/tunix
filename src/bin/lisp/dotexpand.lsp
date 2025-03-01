(or (macro? do)
    (load 'do.lsp))
(or (cons? every)
    (load 'every.lsp))

(fn %des (%dex)
  (? (and (not (every '((%dex) (== \. %dex)) %dex))
          (member \.  %dex)
          (not (member \  %dex)))
     (?
       (eql \. (car (last %dex)))
         $(car ,(%des (butlast %dex)))
       (eql \. (car %dex))
         $(cdr ,(%des (cdr %dex)))
       (let (%dex (split \. %dex))
         $(cdr (assoc ',(symbol (car (last %dex)))
                      ,(%des (apply append (pad (.. \.) (butlast %dex))))))))
     (symbol %dex)))

(fn %debq (%dex)
  (?
    (and %dex (symbol? %dex))
      (%des (symbol-name %dex))
    (atom %dex)
      %dex
    (eq 'unquote (car %dex))
      (.. 'unquote (dotexpand (cadr %dex)))
    (. (%debq (car %dex))
       (%debq (cdr %dex)))))

(fn dotexpand (%dex)
  (?
    (and %dex (symbol? %dex))
      (%des (symbol-name %dex))
    (or (atom %dex)
        (eq 'quote (car %dex)))
      %dex
    (eq 'backquote (car %dex))
      (.. 'backquote (%debq (cadr %dex)))
    (. (dotexpand (car %dex))
       (dotexpand (cdr %dex)))))

(= *ex* '((%dex) (macroexpand (dotexpand %dex))))
