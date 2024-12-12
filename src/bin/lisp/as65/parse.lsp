(load 'as65/package.lsp)

(var *all-mnem65* (append (apply append *mn-6502*)
                          (remove t (remove nil (apply append (remove t (remove nil (apply append *6502*))))))))

(fn labeldef? (x)
  (!= (slength x)
    (and (< 1 !)
         (== \: (char-at x (-- !))))))

(fn mnem? (x)
  (member x *all-mnem65*))

(or (mnem? 'lda)
    (error "MNEM? not working"))

; https://github.com/SvenMichaelKlose/tunix/issues/13
(require 'when)

(fn parse (x)
  ; "Returns (next-x . alist-info)"
  (block nil
    (awhen (car x)
      (unless (symbol? !)
        (error "Symbol expected"))

      (when (labeldef? !)
        (return (. (cdr x)
                   (list (. 'label (symbol (butlast (symbol-name !))))))))
      (and (symbol? !)
           (eq ': (cadr x))
           (return (. (cddr x)
                      (list (. 'label !)))))

      (or (mnem? !)
          (error "Menomic expected"))
      (let (desc nil)
        (acons! 'mnem ! desc)
        (= x (cdr x))

        (awhen (car x)
          (when (eq ! '#)
            (return $(,(cddr x)
                      ,@desc
                      (mode . imm)
                      ,(. 'op (cadr x)))))
          (when (cons? !)   ; (a) / (a,x) / expr
            (and (== 2 (length !)) ; Lisp expression
                 (cons? (cadr !)) ; (a,x) -> (a (quote x))
                 (eq 'unquote (car (cadr !)))
                 (eq 'x (cadr (cadr !)))
                 (return $(,(cdr x)
                           ,@desc
                           (mode . izpx)
                           ,(. 'op (car !)))))
            (acons! 'mode 'ind desc)
            (acons! 'op (car !) desc))
          (unless (cons? !)
            (acons! 'mode 'abs desc)
            (acons! 'op ! desc))
          (= x (cdr x))
          (awhen (car x)
            (or (eq 'unquote (car !))
                (error "QUOTE or eol"))
            (!= (cadr !)
              (or (eq 'x !)
                  (eq 'y !)
                  (error ",x or ,y!"))
              (acons! 'ireg ! desc))
            (= x (cdr x))))
        (. x desc)))))

(in-package nil)
