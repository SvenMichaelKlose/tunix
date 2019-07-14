hex

vocabulary vicrel immediate
vicrel definitions

: init   ( -- )
    ( set DDR and clear all channels )
    3f  9112  c!  00  9110  c!  ;

: channel@   ( n  --  tf )
    ( fetch the value of a channel, 0-7 )
    1  swap  lshift  9110  c@  and  0=  not  ;

: channel!   ( tf n  -- )
    ( store a value to a channel, 0-5 )
    1  swap  lshift  swap  if
	( set bit )
	9110  c@  or  9110  c!  else
	( clear bit )
	not  9110  c@  and  9110  c!  then  ;

init

forth definitions
decimal
