hex

vocabulary graphics immediate
graphics definitions

( Colour constants )

0 constant black
1 constant white
2 constant red
3 constant cyan
4 constant purple
5 constant green
6 constant blue
7 constant yellow

8  constant orange
9  constant lt-orange
0a constant pink
0b constant lt-cyan
0c constant lt-purple
0d constant lt-green
0e constant lt-blue
0f constant lt-yellow

( Mode flags )

1 constant double-height
2 constant bitmap
8 constant multi-colour

( Set the border colour )
: border   ( col8  --  )
    7  and  900f  c@  0f8  and  or  900f  c!  ;

( Set the background colour )
: background   ( col16  --  )
    4  lshift  900f  c@  0f  and  or  900f  c!  ;

( Set the auxillary colour )
: aux   ( col16  --  )
    4  lshift  900e  c@  0f  and  or  900e  c!  ;

( Set the foreground colour )
: foreground   ( col8  --  )
    286  c!  ;

( Set the global reverse video flag )
: invert   ( tf  --  )
    0=  8  and  900f  c@  0f7  and  or  900f  c!  ;

( Return TRUE if NTSC, FALSE if PAL )
: ?ntsc   (  --  flag )
    0e475  C@  41  =  ;

( Convert a VIC address to a system address )
: vic2cpu   ( vicaddr  --  cpuaddr )
    dup  2000  and  if
	1fff  and  else
	8000  or  then  ;

( Convert a system addres to a VIC address )
: cpu2vic   ( cpuaddr  --  vicaddr )
    dup  8000  and  if
	3fff  and  else
	2000  or  then  ;

( Get the start of screen address )
: screenbase@   (  --  addr )
    9005  c@  0f0  and  6  lshift  9002  c@  80  and  2  lshift  or  vic2cpu  ;

( Get the start of character generator address )
: charbase@   (  --  addr )
    9005  c@ 0f  and  0a  lshift  vic2cpu  ;

( Get the start of colour attribute address )
: colourbase@   (  --  addr )
    9002  c@  80  and  2  lshift  9400  or  ;

( Get the cell X and Y limits )
: xymax@   (  --  maxx maxy )
    9002  c@  7f  and  9003  c@  7e  and  2/  ;

( Get the screen position co-ordinates )
: xypos@   (  --  posx posy )
    9000  c@  7f  and  9001  c@  ;

( Set the start of screen address )
: screenbase!   ( addr  --  )
    cpu2vic  dup  6  rshift  0f0  and  9005  c@  0f  and  or  9005 c!
    2  rshift  80  and  9002  c@  7f  and  or  9002  c!  ;

( Set the start of character generator address )
: charbase!   ( addr  --  )
    cpu2vic  0a  rshift  0f  and  9005  c@  0f0  and  or  9005  c!  ;

( Set the cell X and Y limits )
: xymax!   ( maxx maxy  --  )
    2*  7e  and  9003  c@  81  and  or  9003  c!
    9002  c@  80  and  or  9002  c!  ;

( Set the screen position co-ordinates )
: xypos!   ( posx posy  --  )
    9001  c!  7f and  9000  c@  80  and  or  9000  c!  ;

( mode field offsets )
( 0 - flags )
1 constant .screenbase
3 constant .charbase
5 constant .maxx
6 constant .maxy
7 constant .posx
8 constant .posy
9 constant .xshift

( Return the cell co-ordinates of pixel co-ordinates )
: xycell   ( pixx pixy flags  --  cellx celly )
    >r  r@  bitmap  and  if
	3  r@  double-height  and  +  rshift
	swap  3  r>  multi-colour  and  if  1-  then  rshift
    swap  else  r>  drop  then  ;

( Calculate the screen position co-ordinates from X and Y limits )
: centre-screen   ( maxx maxy flags  --  posx posy )
    >r  >r  ?ntsc  if  1b  else  22  then  swap  -
    ?ntsc  if  47  else  54  then  r>  2*  r>  double-height  and  if  2*  then  -  ;

( Create a mode object using the current screen and character generator addresses )
: mode   ( maxx maxy flags  --  )
    2  pick  2  pick  2  pick  create  >r  r@
    c,  screenbase@  ,  charbase@  ,  swap  c,  c,
    dup  >r  xycell  r>  centre-screen  swap  c,  c,
    3  r>  multi-colour  and  if  1-  then  c,  ;

( Address of the active mode )
variable current-mode

( Return the cell X and Y limits of a mode object )
: xycellmax   ( mode  --  cellx celly )
    dup  .maxx  +  c@  over  .maxy  +  c@  rot  c@  xycell  ;

( Initialize the VIC registers and set the current mode )
: mode-set   ( mode  --  )
    >r  r@  current-mode  !
    r@  .screenbase  +  @  screenbase!  r@  .charbase  +  @  charbase!
    r@  xycellmax  xymax!  r@  .posx  +  c@  r@  .posy  +  c@  xypos!
    r>  c@  double-height  and  9003  c@  0fe  and  or  9003  c!  ;

( Get the start of colour attribute address of a mode object )
: colourbase   ( mode  --  addr )
    .screenbase  +  @  200  and  9400  or  ;

( Fill the screen with a sequence of characters to use as a bitmap )
: tile-screen   (  --  )
    current-mode  @  dup  .screenbase  +  @  swap  xycellmax  0
    2  pick  0  do
	over  0  do
	    dup  3  pick  i  *  j  +  5  pick  +  c!  1+
	loop
    loop  2drop  2drop  ;

( Fill the colour attributes with the foreground colour )
: fill-colour   (  --  )
    current-mode  @  dup  colourbase  swap  dup  xycellmax  *  swap
    c@  multi-colour  and  286  c@  or  fill  ;

( Fill the character generator memory with zeros )
: zero-bmap   (  --  )
    current-mode  @  dup  .charbase  +  @  swap  dup  xycellmax  *
    swap  c@  8  swap  double-height  and  if  2*  then  *  erase  ;

( Fill the screen memory with spaces )
: blank-screen   (  --  )
    current-mode  @  dup  .screenbase  +  @  swap  xycellmax  *  blank  ;

( Present an empty text or graphics screen )
: clear-screen   (  --  )
    current-mode  @  c@  bitmap  and  if
	( graphics mode )
	zero-bmap  tile-screen
	else  blank-screen
    then  fill-colour  ;

( Return the cell address for cell X and Y co-ordinates )
: char-at   ( x y  --  addr )
    current-mode  @  .maxx  +  c@  *  +
    current-mode  @  .screenbase  +  @  +  ;

( Return the colour attribute address for cell X and Y co-ordinates )
: colour-at   ( x y  --  addr )
    current-mode  @  >r  r@  c@  xycell  r@  .maxx  +  c@  0  r@  c@
    xycell  drop  *  +  r>  colourbase  +  ;

( Return the character generator offset for pixel X and Y co-ordinates )
: byteof   ( y x  --  offset )
    current-mode  @  >r  r@  .xshift  +  c@  rshift  r>  .maxy  +  c@  *  +  ;

create upbits
80  c,  40  c,  20  c,  10  c,  8  c,  4  c,  2  c,  1  c,

create masks
7f  c,  0bf  c,  0df  c,  0ef  c,  0f7  c,  0fb  c,  0fd  c,  0fe  c,
3f  c,  3f  c,  0cf  c,  0cf  c,  0f3  c,  0f3  c,  0fc  c,  0fc  c,

( Return the update value and mask for a colour index at a pixel X co-ordinate )
: maskfor   ( i x  --  update mask )
    7  and  >r  r@
    current-mode  @  c@  multi-colour  and  if
	( two bit colour index )
	3  and  3  swap  -  2*  lshift  r>  8  +  else
	( single bit, background/foreground )
	swap  if
	    upbits  +  c@  else
	    0  then
	r>  then
    masks  +  c@  ;

( Plot a colour index at pixel X and Y co-ordinates )
: plot   ( i x y  --  )
    over  byteof  current-mode  @  .charbase  +  @  +  >r  r@  c@
    >r  maskfor  r>  and  or  r>  c!  ;

( Character set addresses in character generator ROM )
8000 constant charset1
8800 constant charset2

( Copy character definitions to character generator memory )
: setchar   ( addr start count  --  )
    3  current-mode  @  c@  double-height  and  +
    >r  r@  lshift  swap  r>  lshift  current-mode  @  .charbase  +
    @  +  swap  cmove  ;


( Restore screen to defaults )
: reset-screen
    0e5c3  sys  blue  foreground  ;

' reset-screen  ex-error  catch

forth definitions
decimal
