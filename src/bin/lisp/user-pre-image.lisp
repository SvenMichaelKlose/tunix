(and (eq +vb+ "smk_editor")
     (@ '((x)
           (or (load (symbol (append (symbol-name x)
                                     (symbol-name ".lisp"))))
               (error "Can't load " x ".lisp")))
        '("when" "unless" "dolist" "alet" "aif" "while" "awhile"
          "queue" "with-queue" "dotimes" "incdec" "nth" "nthcdr"
          "awhen" "nthcdr" "subseq" "mapcar" "mapcan" "case"
          "group" "cbm-keycode" "cbm-con" "ls")))
