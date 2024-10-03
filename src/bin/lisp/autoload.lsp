(load "let.lsp")
(load "when.lsp")
(load "progn.lsp")
(load "mapcar.lsp") ; Workaround

(var *alv?* nil)
(var *alx*
  '((!? . aif)))

(fn %aload (%n)
  (let %f (symbol (nconc (symbol-name (or (cdr (assoc %n *alx*)) %n))
                         (symbol-name ".lsp")))
    (let %! (open %f 'r)
      (when %!
        (close %!)
        (load %f)))))

(fn %al (%code %top %x)
  (block nil
    (when (and (== %code 5) ; ERROR_NOT_FUNCTION
               (%aload (car %x)))
      (when (macro? (car %x))
        (let %m (macroexpand %x)
          (setcar %x (car %m))
          (setcdr %x (cdr %m))))
      (return (eval %x)))
    '%fail))

(fn autoload (%code %top %x)
  (let %v? *v?*)
    (= *v?* *alv?*)
    (let %! (%al %code %top %x)
      (= *v?* %v?)
      %!))

(= onerror autoload)
