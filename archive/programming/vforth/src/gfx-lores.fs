include? graphics graphics.fs

graphics definitions

vocabulary lores immediate
lores definitions

hex

1000 constant cstart


( Clear screen )
: clear  (  --  )
    93  emit  colourbase@  200  286  c@  fill  ;

( Calculate cell address )
: getclad  ( x y  --  caddr )
    2/  cellx  *  swap  2/  +  cstart  +  ;

create cmask
1 c,  2  c,  4 c,  8 c,

( Calculate bitmask from X, Y )
: constm  ( x y  --  bm )
    1  and  2*  swap  1  and  or  cmask  +  c@  ;

create cchar
20 c,  7e c,  7c c,  0e2 c,  7b c,  61 c,  0ff c,  0ec c,
6c c,  7f c,  0e1 c,  0fb c,  62 c,  0fc c,  0fe c,  0a0 c,

( Calculate bitmask from current character )
: char2m  ( c  --  bm )
    0  swap  10 0 do
	dup  i  cchar  +  c@  =  if
	    nip  i  swap  leave  then
	loop
	drop  ;

( Set a pixel at X, Y )
: plot  ( x y  --  )
    2dup  constm  >r
    getclad  dup  c@  char2m  r>  or
    cchar  +  c@  swap  c!  ;

( Clear a pixel at X, Y )
: unplot  ( x y  --  )
    2dup  constm  not  >r
    getclad  dup  c@  char2m  r>  and
    cchar  +  c@  swap  c!  ;

( Invert a pixel at X, Y )
: xplot  ( x y  --  )
    2dup  constm  >r
    getclad  dup  c@  char2m  r>  xor
    cchar  +  c@  swap  c!  ;

( Get colour attribute address )
: getcad  ( x y  --  caddr )
    2/  cellx  *  swap  2/  +  colourbase@  +  ;

( Set foreground colour )
: fgset  ( col8 x y  --  )
    getcad  c!  ;

forth definitions
decimal
