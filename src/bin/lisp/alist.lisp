(or (cons? find-if)
    (load "find-if.lisp"))
(or (macro? 'push)
    (load "stack.lisp"))

(fn assoc (k x)
  (do ((i x (cdr i)))
      ((not i))
    (? (eql k (caar i))
       (return (car i)))))

(message "Testing ASSOC...")
(or (assoc 'vic '((c64 t)(vic t)))
    (error))
(and (assoc 'vic '((c64 t)(amiga t)))
     (error))

(macro acons (k v l)
  $(push (cons ,k ,v) ,l))

(message "Testing ACONS...")
(or (let x nil
      (acons 'vic 20 x)
      (equal x '((vic . 20))))
    (error))
