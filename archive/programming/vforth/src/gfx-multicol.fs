include? graphics graphics.fs

graphics definitions

vocabulary multicol immediate
multicol definitions

hex


( Clear screen )
: clear  (  --  )
    bitclear  scrntile  286  c@  multi  or  fgfill  ;

( Initialize multicolour graphics )
: init  (  --  )
    regset  clear  ;

( Calculate bitmap address )
: getbad  ( y x  --  baddr )
    2  rshift  0c0  *  +  bmstart  +  ;

: bmask
    create  c,  c,  c,  c,
    does>  swap  3  and  +  c@  ;

 3  0c  30  0c0  bmask  bpair

( Set a pixel to foreground colour at X, Y )
: plot  ( x y  --  )
    over  getbad  swap  bpair  over  c@  over  not  and  swap  0aa  and  or  swap  c!  ;

( Set a pixel to border colour at X, Y )
: brdplot  ( x y  --  )
    over  getbad  swap  bpair  over  c@  over  not  and  swap  055  and  or  swap  c!  ;

( Set a pixel to auxiliary colour at X, Y )
: auxplot  ( x y  --  )
    over  getbad  swap  bpair  over  c@  over  not  and  or  swap  c!  ;

( Set a pixel to background colour at X, Y )
: bgplot  ( x y  --  )
    over  getbad  swap  bpair  not  over  c@  and  swap  c!  ;

( Get colour attribute address )
: getcad  ( x y  --  caddr )
    4  rshift  bmcellx  *  swap  2  rshift  +  colourbase@  +  ;

( Set foreground colour )
: fgset  ( col8 x y  --  )
   getcad  swap  multi  or  swap  c!  ;

forth definitions
decimal
