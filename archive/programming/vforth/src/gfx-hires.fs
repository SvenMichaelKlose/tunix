include? graphics graphics.fs

graphics definitions

vocabulary hires immediate
hires definitions

hex


( Clear screen )
: clear  (  --  )
    bitclear  scrntile  286  c@  fgfill  ;

( Initialize hires graphics )
: init  (  --  )
    regset  clear  ;

( Calculate bitmap address )
: getbad  ( y x  --  baddr )
    3  rshift  0c0  *  +  bmstart  +  ;

create bmask
80 c,  40 c,  20 c,  10 c,  8 c,  4 c,  2 c,  1 c,

( Set a pixel at X, Y )
: plot  ( x y  --  )
    over  getbad  >r  7  and  bmask  +  c@  r@  c@  or  r>  c!  ;

( Clear a pixel at X, Y )
: unplot  ( x y  --  )
    over  getbad  >r  7  and  bmask  +  c@  not  r@  c@  and  r>  c!  ;

( Invert a pixel at X, Y )
: xplot  ( x y  --  )
    over  getbad  >r  7  and  bmask  +  c@  r@  c@  xor  r>  c!  ;

( Get colour attribute address )
: getcad  ( x y  --  caddr )
    4  rshift  bmcellx  *  swap  3  rshift  +  colourbase@  +  ;

( Set foreground colour )
: fgset  ( col8 x y  --  )
    getcad  c!  ;

( Load MINIGRAFIK image from file )
: mgload  ( addr count  --  )
    0  open-file  ?ioerr  >r
    pad  11  r@  read-file  ?ioerr  drop  ( read load addr, stub and reg )
    900e  c@  0f  and  pad  0f  +  c@  or  900e  c!  ( restore VRE )
    pad  10  +  c@  900f  c!  ( restore VRF )
    bmstart  0f00  r@  read-file  ?ioerr  drop  ( restore bitmap )
    pad  78  r@  read-file  ?ioerr  ( read colour attributes )
    colourbase@  swap  0  do
	pad  i  +  c@  2dup  swap  c!  ( low nibble )
	4  rshift  over  1+  c!  2+  ( hi nibble )
    loop  drop
    r>  close-file  ?ioerr  ;

forth definitions
decimal
