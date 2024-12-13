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
    (awhen x.
      (unless (symbol? !)
        (error "Symbol expected"))

      (when (labeldef? !)
        (return (. .x
                   (list (. 'label (symbol (butlast (symbol-name !))))))))
      (and (symbol? !)
           (eq ': .x.)
           (return (. ..x
                      (list (. 'label !)))))

      (or (mnem? !)
          (error "Menomic expected"))
      (let (desc nil)
        (acons! 'mnem ! desc)
        (= x .x)

        (awhen x.
          (when (eq ! '#)
            (return $(,..x
                      ,@desc
                      (mode . imm)
                      ,(. 'op .x.))))
          (when (cons? !)   ; (a) / (a,x) / expr
            (and (== 2 (length !)) ; Lisp expression
                 (cons? .!.) ; (a,x) -> (a (unquote x))
                 (eq 'unquote (car .!.))
                 (eq 'x (cadr .!.))
                 (return $(,.x
                           ,@desc
                           (mode . izpx)
                           ,(. 'op !.))))
            (acons! 'mode 'ind desc)
            (acons! 'op !. desc))
          (unless (cons? !)
            (acons! 'mode 'abs desc)
            (acons! 'op ! desc))
          (= x .x)
          (awhen x.
            (or (eq 'unquote !.)
                (error "QUOTE or eol"))
            (!= .!.
              (or (eq 'x !)
                  (eq 'y !)
                  (error ",x or ,y!"))
              (acons! 'ireg ! desc))
            (= x .x)))
        (. x desc)))))

(in-package nil)
