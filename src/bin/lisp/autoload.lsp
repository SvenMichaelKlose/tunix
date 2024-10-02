(load "let.lsp")
(load "when.lsp")
(load "progn.lsp")
(load "do.lsp")
(load "dolist.lsp")

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
  (? (== %code 5) ; ERROR_NOT_CALLABLE
     (progn
       (%aload (car %x))
       (when (macro? (car %x))
         (let %m (macroexpand %x)
           (setcar %x (car %m))
           (setcdr %x (cdr %m))))
       (eval %x))
     '%fail))

(fn autoload (%code %top %x)
  (let %v? *v?*)
    (= *v?* nil)
    (let %! (%al %code %top %x)
      (= *v?* %v?)
      %!))

(= onerror autoload)
(= *macros* nil)
