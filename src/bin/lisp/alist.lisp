(fn assoc (k x)
  (do ((i x (cdr i)))
      ((not i))
    (? (eql k (caar i))
       (return (car i)))))

(macro acons (k v l)
  $(push (cons ,k ,v) ,l))
