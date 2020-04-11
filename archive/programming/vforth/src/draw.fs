vocabulary draw immediate
draw definitions

variable lastx
0 lastx !
variable lasty
0 lasty !

variable plotw

variable x1
variable y1
variable dx
variable dy
variable sx
variable sy
variable err

: goto    ( x y  --  )
    lasty  !  lastx  !  ;

: lineto    ( x y  --  )
    y1  !  x1  !
    x1  @  lastx  @  -  abs  dx  !
    lastx  @  x1  @  <  if  1  else  -1  then  sx  !
    y1  @  lasty  @  -  abs  dy  !
    lasty  @  y1  @  <  if  1  else  -1  then  sy  !
    dx  @  dy  @  >  if  dx  @  else  dy  @  negate  then  2/  err  !
    begin
	lastx  @  lasty  @  plotw  @  execute   ( plot point )
	lastx  @  x1  @  xor  lasty  @  y1  @  xor  or  while
	    err  @  dup  dx  @  negate  >  if  dy  @  negate err  +!  sx  @  lastx  +!  then
	    dy  @  <  if  dx  @  err  +!  sy  @  lasty  +!  then
    repeat ;

forth definitions
