(fn assoc (k x)
  (find-if $((e)
              (eql ,k e))
           x))

(macro acons (k v l)
  $(push ,l (cons ,k ,v)))
