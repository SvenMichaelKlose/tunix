hex

vocabulary gameio immediate
gameio definitions

: joy   ( -- xd yd fire )
    1  9122  c@  dup  7f  and  9122  c!  ( VIA2 DDRB b7 as input )
    9120  c@  ( VIA2 DRB )
    swap  9122  c!  ( restore VIA2 DDRB )
    80  and  if  1-  then
    9111  c@  >r  r@  10  and  0=  if  drop  -1  then
    0  r@  4  and  0=  if  1-  else  r@  8  and  0=  if  1+  then  then
    1  r>  20  and  if  1-  then  ;

: paddlex   ( -- pos fire )
    9008  c@
    1  9111  c@  10  and  if  1-  then  ;

: paddley   ( -- pos fire )
    9009  c@  1
    9122  c@  dup  7f  and  9122  c!  ( VIA2 DDRB b7 as input )
    9120  c@  ( VIA2 DRB )
    swap  9122  c!  ( restore VIA2 DDRB )
    80  and  if  1-  then  ;

: lightpen   ( -- x y down )
    9006  c@  9007  c@
    1  9111  c@  20  and  if  1-  then  ;

forth definitions
decimal
