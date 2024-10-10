(var *con-w* 80)
(var *con-h* 25)
(var +enter+ 13)

(when (member +target+ '(c128 c16 c64 pet plus4 vic20))
  (var +arr-up+ 145)
  (var +arr-down+ 17)
  (var +arr-left+ 157)
  (var +arr-right+ 29)
  (var +bs+ 20)
  (var +del+ 20)
  (var +ins+ 148)
  (when (eq +target+ 'pet)
    (= *con-w* 80))
  (when (eq +target+ 'vic20)
    (= *con-w* 22)
    (= *con-h* 23)))

(fn clrscr ()
  (out 12))

(fn con-xy (x y)
  (out 1)
  (out (++ x))
  (out (++ y)))

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
  (con-get 5))

(fn con-y ()
  (con-get 6))
