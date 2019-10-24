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

( Set a pixel at x, y )
: plot  ( x y  --  )
    over  getbad  >r  7  and  bmask  +  c@  r@  c@  or  r>  c!  ;

( Clear a pixel at x, y )
: unplot  ( x y  --  )
    over  getbad  >r  7  and  bmask  +  c@  not  r@  c@  and  r>  c!  ;

( Invert a pixel at x, y )
: xplot  ( x y  --  )
    over  getbad  >r  7  and  bmask  +  c@  r@  c@  xor  r>  c!  ;

( Get colour attribute address )
: getcad  ( x y  --  caddr )
    4  rshift  cellx  *  swap  3  rshift  +  colourbase@  +  ;

( Set foreground colour )
: fgset  ( col8 x y  --  )
    getcad  c!  ;

forth definitions
decimal
