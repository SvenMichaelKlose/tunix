(macro defsetfn (name . body)
  $(fn ,name (a b)
     ,@body ))
