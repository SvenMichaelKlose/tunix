: case
    ?comp  csp  @  !csp  4  ;   immediate

: of
    4  ?pairs  compile over  compile  =  compile  ?branch  here  0  ,
    compile  drop  5  ;   immediate

: endof
    5  ?pairs  compile  branch  here  0  ,  swap  2  [compile]  then
    4  ;   immediate

: endcase
    4  ?pairs  compile  drop  begin  sp@  csp  @  =  0=  while
	    2  [compile]  then  repeat  csp  !  ;   immediate
