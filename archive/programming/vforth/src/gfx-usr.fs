include? graphics graphics.fs

graphics definitions
hex

vocabulary usr immediate
usr definitions

( RAM address for character generator )

variable charbase
1400 charbase !

( Clear screen )
: clear  (  --  )
    93  emit  ;

( Copy a character set to RAM )
: redefset  ( addr  --  )
    charbase  @  ramhi  over  -  cmove
    charbase  @  charbase!  ;

( Redefine a number of characters )
: redef  ( addr ch count  --  )
    3  lshift  >r  3  lshift  charbase  @  +  r>  cmove  ;

( Copy a character set to RAM, convert to double height )
: redefset2  ( addr  --  )
    ramhi  charbase  @  -  2/  0  do
	dup  i  +  c@  charbase  @  i  2*  +  2dup
	c!  1+  c!
	loop  drop
    charbase  @  charbase!  16  cellh!  ;

( Redefine a number of double height characters )
: redef2  ( addr ch count  --  )
    4  lshift  >r  4  lshift  charbase  @  +  r>  cmove  ;

forth definitions
decimal
