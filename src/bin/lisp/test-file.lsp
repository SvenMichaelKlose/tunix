(load "prog1.lsp")
(load "!=.lsp")
(load "with.lsp")
(load "with-in.lsp")
(load "with-out.lsp")

(message "Testing OPEN on missing file...")
(and (open "k87sfdasdj9" 'r)
     (error "File \"k87sfdasdj9\" should be missing."))

(= *alv?* nil)
(message "AUTOLOAD muted.")

(message "Testing OPEN file write to \"test.out\"...")
(awhen (with-out o (open "test.out" 'w)
         (print message))
  (error !))

(message "Testing OPEN file read from \"test.out\"...")
(awhen (with-in i (open "test.out" 'r)
         (!= (read)
           (or (equal ! message)
               (error "Expression read from \"test.out\" does not match MESSAGE: " !))))
  (error !))

(message "Testing READ/PRINT copy...")
(message "Copying...")
(awhen (with-in i (open "test.out" 'r)
         (awhen (with-out o (open "test2.out" 'w)
                  (print (read)))
           (error !)))
  (error !))
(message "Verifying...")
(awhen (with-in i (open "test2.out" 'r)
         (!= (read)
           (or (equal ! message)
               (error "Unexpected expression read from \"test2.out\": " !))))
  (error !))
