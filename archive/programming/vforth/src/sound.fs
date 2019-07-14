hex

vocabulary sound immediate
sound definitions

900a constant voice1
900b constant voice2
900c constant voice3
900d constant noise
0 constant vol


: volume   ( vol -- )
    7  and  900e  c@  0f0  and  or  900e  c!  ;

: tune   ( nnnn )
    create  here  6  +  ,  0.  ,  ,  ;

: note,   ( delta val reg -- )
    rot  ,  swap  c,  c,  ;

: end-tune
    -1  ,  ;

: play-notes   ( tune -- )
    >r  time@  begin
	2dup  r@  2+  @  r@  4  +  @
	r@  @  @  0  d+  d<  if
	    2drop  r>  drop  exit  then
	r@  @  2+  dup  c@  swap  1+  c@  ?dup  if
	    9000  or  c!  else
	    volume  then
	r@  @  4  +  r@  !
	again  ;

: play   ( tune -- tf )
    >r  r@  2+  @  r@  4  +  @  or  0=  if
	time@  r@  4  +  !  r@  2+  !  then
    r@  play-notes
    r@  @  @  -1  =  if
	0.  r@  2+  !  r@  4  +  !
	r@  6  +  r>  !  true  else
	r>  drop  false  then  ;

: reset-sound
    0  volume  0  voice1  c!  0  voice2  c!
    0  voice3  c!  0  noise  c!  ;

' reset-sound  ex-error  catch

forth definitions
decimal
