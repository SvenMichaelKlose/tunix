(fn source (x)
  (? (symbol? x)
     (let (c nil
           v (symbol-value x))
       $(,(?
            (special? x) 'special
            (macro? x) 'macro
            (cons? v) 'fn
            'var)
         ,x ,@v))
     x))
