include draw.fs
include gfx-hires.fs

graphics hires  ' xplot  draw  plotw  !


variable rndseed
1 rndseed !

: rnd
    rndseed  @  dup  13  lshift  xor  dup  9  rshift  xor
    dup  7  lshift  xor  dup  rndseed  !  ;

: demo
    graphics hires  init
    200  0  do
	rnd  160  mod  rnd  192  mod  draw  lineto
    loop
    key  drop
    graphics hires  reset-screen
    147  emit  ;
