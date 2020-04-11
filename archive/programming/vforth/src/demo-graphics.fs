include gfx-usr.fs

create ship
0  c,  24  c,  60  c,  126  c,  255  c,  24  c,  36  c,  66  c,

: ships
    graphics  usr
    charset1 redefset
    ship  30  1  redef
    cr  20  0  do
	94  emit
    loop  cr  ;


include gfx-lores.fs

: shape
    graphics lores
    clear
    30 0 do
	10  i  +  10  plot
	red  10  i  +  10  fgset
	10  i  +  20  plot
	green  10  i  +  20  fgset
	10  i  +  30  plot
	black  10  i  +  30  fgset
	10  i  +  40  plot
	10  i  +  dup  xplot
	40  i  -  10  i  +  xplot
    loop  ;


include gfx-hires.fs
include trig.fs

: plotsin
    graphics  hires
    init
    200  0  do
        320  0  do
            i  2/  i  j  +  sin  90  10000  */  95  +  plot
        2  +loop
    5  +loop  ;


include gfx-multicol.fs

: lines
    graphics  multicol
    init
    orange  aux
    80  0  do
	i  dup  brdplot
	79  i  -  i  95  +  brdplot
	i dup  5  +  auxplot
	79  i  -  i  100  +  auxplot
	i dup  10  +  plot
	79  i  -  i  105  +  plot
    loop  ;

forth

: demo
    ships  key  drop
    shape  key  drop
    plotsin  key drop
    lines  key  drop
    graphics  reset-screen  lores  clear  ;

forth
