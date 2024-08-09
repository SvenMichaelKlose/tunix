(macro with-global (n v .body)
  ;"Temporarily assign global with new value."
  (let g (symbol)
    $(block t
       (= ,g ,n)
       (= ,n ,v)
       (block nil
         ,@body)
       (= ,n ,g))))

(message "Testing WITH-GLOBAL...")
(let old-macros *macros*
  (with-global *macros* nil
    (and *macros*
         (error "*MACROS* not NIL.")))
    (or *macros*
        (error "*MACROS* is NIL.")))
