(fn clrscr ()
  (out 12))

(fn con-xy (x y)
  (out 1)
  (out x)
  (out y))

(fn con-clrset (x f)
  (out (? x 3 2))
  (out f))

(fn con-crs (x)
  (con-clrset x 1))

(fn con-rvs (x)
  (con-clrset x 2))

(fn con-direct (x)
  (con-clrset x 4))

(fn con-get (x)
  (out x)
  (-- (conin)))

(fn con-x ()
  (con-get 4))

(fn con-y ()
  (con-get 5))
