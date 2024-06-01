; Convert Backus-Naur form to Lisp form.
;
; From:
;   rule-nameA : rule-option | rule-option ;
;   rule-nameB : rule-option | rule-option ;
;
; To:
;   ((rule-nameA ((rule-option)
;                 (rule-option)))
;    (rule-nameB ((rule-option)
;                 (rule-option))))

(fn get-rule ()
  (collection
    (while ((not (next "|" ";"))
            (progn
              (expect ";")
              (collected)))
      (collect (get-rule-item)))))

(fn get-rule-options ()
  (expect ":")
  (collection
    (while ((next)
            (collected))
      (collect (get-rule)))))

(fn bnf2lisp ()
  (. (get-identifier)
     (get-rule-options)))
