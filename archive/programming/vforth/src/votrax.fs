vocabulary votrax immediate
votrax definitions
hex

0a constant 2400baud 

variable lfh

: open  ( baud  --  )
    pad  c!  0  pad  1+  c!  pad  2  $setnam
    nextlfn  lfh  !  0  2  lfh  @  $setlfs  $open  drop  ;

: close  (  --  )
    ( check Tx buffer empty )
    lfh  @  $close  drop  ;

: (say")
    r@  count  dup  1+  r>  +  >r  lfh  @  write-line  ?ioerr  ;

: say"
    compile  (say")  22  word  c@  1+ allot  ;  immediate

: say(
    29  word  count  lfh  @  write-line  ?ioerr  ;  immediate

: esc  ( byte  --  )
    1b  pad  c!  pad  1+  c!  pad  2  lfh  @  write-file  ?ioerr  ;

: echo  ( tf  --  )
    if  13  else  14  then  esc  ;

: caps  ( tf  --  )
    if  15  else  16  then  esc  ;

: psend  ( tf  --  )
    if  11  else  12  then  esc  ;

: reset  (  --  )
    18  esc ;

forth definitions
decimal
