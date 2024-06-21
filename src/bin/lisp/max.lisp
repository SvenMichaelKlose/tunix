(fn max numbers
  ;"Return the maximum of the given numbers."
  (? (not numbers)
     (error "No arguments provided to MAX")
     (let max-so-far (car numbers)
       (dolist (n (cdr numbers) max-so-far)
         (when (> n max-so-far)
           (= max-so-far n))))))
