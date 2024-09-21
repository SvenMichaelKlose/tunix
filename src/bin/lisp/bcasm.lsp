(fn mkbcfun (n)
  (bcsymbol (with-queue q
              (dotimes (i n)
                (push 0 q)))))

(fn bcasm (x)
  (!= (mkbcfun (bclen x))
    (let i 0
      (do ((c x (cdr c)))
          ((not c))
        (let e (car c)
          (? (atom x)
             (error "Unexpected atom " x " in metacode.")
             (case (car e)
               '%bc-end
               (progn
                   (set-char-at ! i 0)
                   (return nil))
               '%bc-go
                 (progn
                   (set-char-at ! i 1)
                   (set-char-at ! (+ 1 i) (cdr (assoc (cadr e) tags)))
                   (= i (+ 2 i)))
               '%bc-go-nil
                 (progn
                   (set-char-at ! i 2)
                   (set-char-at ! (+ 1 i) (cdr (assoc (cadr e) tags)))
                   (= i (+ 2 i)))
               '%bc-go-nnil
                 (progn
                   (set-char-at ! i 3)
                   (set-char-at ! (+ 1 i) (cdr (assoc (cadr e) tags)))
                   (= i (+ 2 i)))
               '%bc-list
                  (progn
                    (set-char-at ! i 4)
                    (= i (+ 1 i))
                    (dotimes (j (cadr e))
                      (= x (cdr x))
                      (let p (rawptr x)
                        (set-char-at ! i (bit-and p 255))
                        (set-char-at ! (++ i) (>> p 8))
                        (= i (+ 2 i))))))
    (=-symbol-value s !)))))))
