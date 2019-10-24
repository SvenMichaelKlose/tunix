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

8 constant multi


( manipulate VIC registers )

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

( Convert a system address to a VIC address )
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

( Get cell height in pixels )
: cellh@  (  --  cellh )
    9003  c@  1  and  if  16  else  8  then  ;

( Get the cell X limit )
: xmax@   (  --  maxx )
    9002  c@  7f  and  ;

( Get the cell Y limit )
: ymax@   (  --  maxy )
    9003  c@  7e  and  2/  ;

( Get the X screen position )
: xpos@   (  --  posx )
    9000  c@  7f  and  ;

( Get the Y screen position )
: ypos@   (  --  posy )
    9001  c@  ;

( Set the start of screen address )
: screenbase!   ( addr  --  )
    cpu2vic  dup  6  rshift  0f0  and  9005  c@  0f  and  or  9005 c!
    2  rshift  80  and  9002  c@  7f  and  or  9002  c!  ;

( Set the start of character generator address )
: charbase!   ( addr  --  )
    cpu2vic  0a  rshift  0f  and  9005  c@  0f0  and  or  9005  c!  ;

( Set the cell height )
: cellh!  ( cellh  --  )
    8  >  1  and
    9003  c@  0fe  and  or  9003  c!  ;

( Set the cell X limit )
: xmax!   ( maxx   --  )
    9002  c@  80  and  or  9002  c!  ;

( Set the cell Y limit )
: ymax!   ( maxy  --  )
    2*  7e  and  9003  c@  81  and  or  9003  c!  ;

( Set the X screen position )
: xpos!   ( posx  --  )
    7f and  9000  c@  80  and  or  9000  c!  ;

( Set the Y screen position )
: ypos!   ( posy  --  )
    9001  c!  ;

( System addresses visible to the VIC )

1000 constant ramlo
2000 constant ramhi

( Character set addresses in character generator ROM )
8000 constant charset1
8800 constant charset2


( Common definitions for bitmaps )
14 constant cellx
0c constant celly  ( double height )

1100 constant bmstart

( Clear bitmap )
: bitclear  (  --  )
    bmstart  0f00  0  fill  ;

( Tile screen )
: scrntile  (  --  )
    cellx  0  do
	celly  0  do
	    10  i  +  j  celly  *  +  i  cellx  *  j  +  ramlo  +  c!
	loop
    loop  ;

( Fill foreground with colour )
: fgfill  ( col8  --  )
    colourbase@  swap  0f0  swap  fill  ;

( Set VIC registers )
: regset  (  --  )
    cellx  xmax!  celly  ymax!  10  cellh!  screenbase@  charbase!  ;


( Restore screen to defaults )
: reset-screen
    0e5c3  sys  blue  foreground  ;

' reset-screen  ex-error  catch

forth definitions
decimal
