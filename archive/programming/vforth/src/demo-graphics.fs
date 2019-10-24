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


include gfx-hires.fs
include trig.fs

: plotsin
    graphics  hires
    init
    200  0  do
        320  0  do
            i  2/  i  j  +  sin  90  10000  */  95  +  plot
        2  +loop
    5  +loop
    key  drop  ;


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
    loop
    key  drop  ;
