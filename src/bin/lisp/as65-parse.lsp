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

(fn as65-parse (x)
  (block nil
    (awhen (car x)
      (unless (symbol? !)
        (error "Symbol expected"))

      (when (labeldef? !)
        (return (. (cdr x) (. 'label !))))
      (and (symbol? !)
           (eq ': (cadr x))
           (return (. (cddr x) (. 'label !))))

      (or (mnem? !)
          (error "Menomic expected"))
      (let (inst nil)
        (acons! 'mnem ! inst)
        (= x (cdr x))

        (awhen (car x)
          (when (eq ! '#)
            (return $(,(cddr x)
                      (mode . imm)
                      ,(. 'op (cadr x)))))
          (when (cons? !)   ; (a) / (a,x) / expr
            (and (== 2 (length !)) ; Lisp expression
                 (cons? (cadr !)) ; (a,x) -> (a (quote x))
                 (eq 'unquote (car (cadr !)))
                 (eq 'x (cadr (cadr !)))
                 (return $(,(cdr x)
                           (mode . indx)
                           ,(. 'op (car !)))))
            (acons! 'mode 'ind inst)
            (acons! 'op ! inst))
          (unless (cons? !)
            (acons! 'op ! inst))
          (= x (cdr x))
          (awhen (car x)
            (or (eq 'unquote (car !))
                (error "QUOTE or eol"))
            (!= (cadr !)
              (or (eq 'x !)
                  (eq 'y !)
                  (error ",x or ,x!"))
              (acons! 'absr ! inst))
            (= x (cdr x))))
        (. x inst)))))
