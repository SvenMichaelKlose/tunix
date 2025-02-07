; ⚠️  UNDER CONSTRUCTION ⚠️

(var *ops*
  '(+ - * / % and or ^ ~ << >> < > !))
(var *tok*
  (append '("(" ")" ",") *ops*))

; Make char number tree from token list.
(var *toktree* '((()))) ; Enable NCONC add.
(dolist (tok *tok*)
  (fn make (tree c)
    (? c
       (list (car c) (make (cdr c)))
       (list nil tok)))
  (fn add (tree c)
    (nconc tree (list (car c) (make (cdr c)))))
  (fn rec (tree c)
    (!? (assoc tree (car c))
        (rec ! c)
        (add tree c)))
  (rec *toktree* (symbol-name tok)))

(print *toktree*)

; Tokensize using char number tree with token symbol leaves.
(fn tokenize ()
  (do* ((c (skip-spaces) (in))
        (tree *toktree* (cdr (or (assoc c tree)
                                 (assoc nil tree))))
        (o (make-queue) (unless (cons? tree)
                          (enqueue o (or tree c))
                          (= tree *toktree*))))
       ((eof) (queue-list o))))
