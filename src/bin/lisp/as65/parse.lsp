(require 'when 'awhen 'acons!)
(or (cons? *6502*)
    (load 'as65/opcode.lsp))
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

(fn parse (x)
  ; "Returns (next-x . alist-info)"
  (block nil
    (awhen x.
      (when (cons? !)
        (return (. .x (list (. 'type 'expr)
                            (. 'data !)))))
      (unless (symbol? !)
        (return (. .x (list (. 'type 'data)
                            (. 'data !)))))

      (when (labeldef? !)
        (return (. .x
                   (list (. 'type 'label)
                         (. 'data (symbol (butlast (symbol-name !))))))))
      (and (symbol? !)
           (eq ': .x.)
           (return (. ..x
                      (list (. 'type 'label)
                            (. 'data !)))))

      (or (mnem? !)
          (error "No mnem: " !))
      (let (desc nil)
        (acons! 'mnem ! desc)
        (acons! 'type 'inst desc)
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
