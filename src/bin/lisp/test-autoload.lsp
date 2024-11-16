(message "Testing auto-loading function argument...")
(reset!)
(gc)
(apply mapcar '((1 2 3)))
