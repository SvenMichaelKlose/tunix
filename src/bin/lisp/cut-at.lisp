(fn cut-at (n l)
  "Destructively cut L at position N and return the cut off tail."
  (? (== n 0)
     l
     (let end (nthcdr (-- n) l)
       (let next (cdr end)
         (and end (setcdr end nil))
         next))))

(message "Testing cut-AT...")
(let head '(l i s p)
  (or (equal (cut-at 2 head) '(s p))
      (error))
  (or (equal head '(l i))
      (error)))
