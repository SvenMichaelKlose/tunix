include? case case.fs

: demo
    begin
	key
	case
	    65  of  ." a"  endof
	    66  of  ." b"  endof
	    67  of  ." c"  endof
	    ." ?"
	endcase
	?terminal  until  ;
