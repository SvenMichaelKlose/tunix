decimal

vocabulary printer immediate
printer definitions

variable prnfn
variable outdev

: open   ( dev# -- )
    0  0  $setnam  0  swap  nextlfn  prnfn  !  prnfn  @  $setlfs  $open  ?ioerr
    prnfn  @  $chkout  ?ioerr  outdev  !  ;

: close
    outdev  @  $rstout  prnfn  @  $close  ;

: rvs-on
    18  emit  ;

: rvs-off
    146  emit  ;

: wide-on
    14  emit  ;

: wide-off
    15  emit  ;

: business
    17  emit  ;

: graphic
    145  emit  ;

: pos   ( n -- )
    16  emit  10  /mod  [char] 0  +  emit  [char] 0  +  emit  ;

: repeat-bit   ( bit n -- )
    8  emit  26  emit  emit  128  or  emit  ;

: dot-pos   ( n -- )
    27  emit  16  emit  256  /mod  emit  emit  ;

: image   ( addr n -- )
    8  emit  0  do
	dup  c@  128  or  emit  1+  loop  drop  ;

forth definitions
