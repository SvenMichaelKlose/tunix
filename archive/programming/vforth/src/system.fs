HEX

030C CONSTANT SAREG
030D CONSTANT SXREG
030E CONSTANT SYREG
030F CONSTANT SPREG

( Enter BASIC, perform cold start skipping EXROM checking )

CODE BASIC   (  --  )
    0FF # LDX,  SEI,  TXS,  CLD,  0FD2F JMP,  ( NO RETURN )
    LABEL JMPSYS
        N ) JMP,
END-CODE

( Call machine code subroutine, registers are taken from SAREG, SXREG, SYREG and SPREG and )
( stored back there on return )

CODE SYS   ( addr  --  )
    1 # LDA,  SETUP JSR,  XSAVE STX,   ( Store call address )
    SXREG LDX,  SYREG LDY,  SPREG  LDA,  PHA,  SAREG LDA,  PLP,
    JMPSYS JSR,
    PHP,  SAREG STA,  PLA,  SPREG STA,  SYREG STY,  SXREG STX,  XSAVE LDX,  NEXT JMP,
END-CODE

( Return jiffy clock as a double )

CODE TIME@   (  --  d )
    DEX,  DEX,  SEI,
    0A2 LDA,  BOT STA,  0A1 LDA,  BOT 1+ STA,  0A0 LDA,
    CLI,  PUSH0A JMP,
END-CODE


( Set jiffy clock from double )

CODE TIME!   ( d  --  )
    SEI,
    SEC LDA,  0A2 STA,  SEC 1+ LDA,  0A1 STA,  BOT LDA,  0A0 STA,
    CLI,  POPTWO JMP,
END-CODE


( Move screen co-ordinates )

CODE $PLOT   ( x y  --  )
    BOT LDA,  SEC LDY,  XSAVE STX,  TAX,  CLC,  0FFF0 JSR,
    XSAVE LDX,  POPTWO JMP,
END-CODE


( Low-level device interface )

( Call KERNAL SETNAM function to define file name )
( Prerequisite of $OPEN )

CODE $SETNAM   ( addr count  --  )
    BOT LDA,  PHA,  SEC LDA,  SEC 1+ LDY,  XSAVE STX,  TAX,  PLA,
    0FFBD JSR,  XSAVE LDX,  POPTWO JMP,
END-CODE

( Call KERNAL SETLFS function to define logical file number, )
( device address and secondary address )
( Prerequisite of $OPEN )

CODE $SETLFS   ( sa da lfn  --  )
    BOT LDA,  PHA,  SEC LDA,  SEC 2+ LDY,  XSAVE STX,  TAX,  PLA,
    0FFBA JSR,  XSAVE LDX,  INX,  INX,  POPTWO JMP,

( Return status of last operation, due to a bug in the READST KERNAL )
( function when handling the RS-232 device this is open coded )

LABEL GETST
    0BA LDA,  2 # CMP,  0=   ( Check for RS-232 )
    IF,  297 LDA,  PHA,  0 # LDA,  297 STA,  PLA,
    ELSE,  90 LDA,
    THEN,  RTS,
END-CODE

CODE $READST   (  --  st )
    GETST JSR,  PUSH0A JMP,
LABEL XSTSERR
    XSAVE LDX,

( Push a combination of the return value of a KERNAL call and the I/O status )
(         15 14-8   7-0 )
(         Cb  .A  STATUS/ )
(                 RSSTAT )
LABEL PUSHSERR
    DEX,  DEX,
LABEL PUTSERR
    CS  IF,  80 # ORA,  ELSE,  0 # LDA,  THEN,  BOT 1+ STA,
    GETST JSR,  BOT STA,  NEXT JMP,
END-CODE

( Call KERNAL OPEN function to open a logical file )
( Requires $SETNAM and $SETLFS to have been called )

CODE $OPEN   (  --  serr )
    XSAVE STX,  0FFC0 JSR,  XSTSERR JMP,
END-CODE

( Call KERNAL CLOSE function to close a logical file )

CODE $CLOSE   ( lfn  --  serr )
    XSAVE STX,  BOT LDA,  0FFC3 JSR,  XSAVE LDX,  PUTSERR JMP,
END-CODE

( Call KERNAL CHKOUT function to declare a logical file as output channel )
( All writes - including EMIT - will go to this file until $RSTOUT is )
( called )
( Returns the current output device - usually 3 )
( Prerequisite of $WRITE )

CODE $CHKOUT   ( lfn  --  da serr )
    BOT 1+ STY,  9A LDY,  BOT LDA,  BOT STY,   ( Save current output device )
    XSAVE STX,  TAX,  0FFC9 JSR,  XSTSERR JMP,
END-CODE

( Restore the output channel to a previous device )
( If the current output device is on the serial bus send an UNLISTEN command )

CODE $RSTOUT   ( da  --  )
    XSAVE STX,  99 LDY,  0 # LDA,  99 STA,  0FFCC JSR,  99 STY,
    XSAVE LDX,  BOT LDA,  9A STA,  POP JMP,
END-CODE

( Call KERNAL CHROUT function to send a character to the output channel )
( Requires $CHKOUT to have been called )

CODE $WRITEB   ( 8b  --  serr )
    BOT LDA,  0FFD2 JSR,  CLC,  PUTSERR JMP,
END-CODE

( Call KERNAL CHROUT function to send a sequence of bytes to the output )
( channel )
( Requires $CHKOUT to have been called )

CODE $WRITE   ( addr count  --  serr )
    2 # LDA,  SETUP JSR,
    BEGIN,  BEGIN,
        N CPY,  0= IF,
        N 1+ LDA,  0= NOT IF,
        SWAP  THEN,
        N 2+ )Y LDA,  0FFD2 JSR,  GETST JSR,
        0BF # AND,  0= IF,
        INY,
        ROT  0= UNTIL,   ( Repeat until .Y wraps )
        N 3 + INC,  N 1+ DEC,
        ROT  JMP,   ( Repeat )
    SWAP  THEN,  0 # LDA,
    THEN,  PUSH0A JMP,
END-CODE

( Call KERNAL CHKIN function to declare a logical file as input channel )
( All reads - including QUERY - will go to this file until $RSTIN )
( is called )
( Returns the current output device - usually 0 )
( Prerequisite of $READ )

CODE $CHKIN   ( lfn  --  da serr )
    BOT 1+ STY,  99 LDY,  BOT LDA,  BOT STY,   ( Save current input device )
    XSAVE STX,  TAX,  0FFC6 JSR,  XSTSERR JMP,
END-CODE

( Restore the input channel to a previous device )
( If the current input device is on the serial bus send an UNTALK command )

CODE $RSTIN   ( da  --  )
    XSAVE STX,  9A LDY,  3 # LDA,  9A STA,  0FFCC JSR,  9A STY,
    XSAVE LDX,  BOT LDA,  99 STA,  POP JMP,
END-CODE

( Call KERNAL CHRIN function to receive a sequence of bytes from the input )
( channel )
( Requires $CHKIN to have been called )

CODE $READ   ( addr count1  --  count2 serr )
    2 # LDA,  SETUP JSR,
    DEX,  DEX,  BOT STY,  BOT 1+ STY,   ( zero output count )
    BEGIN,  BEGIN,
        N CPY,  0= IF,
        N 1+ LDA,  0= NOT IF,
        SWAP  THEN,
        0FFCF JSR,  N 2+ )Y STA,
        BOT INC,  0= IF,  BOT 1+ INC,  THEN,
        GETST JSR,
        0= IF,
        INY,
        ROT  0= UNTIL,   ( Repeat until .Y wraps )
    N 3 + INC,  N 1+ DEC,
    ROT  JMP,   ( Repeat )
    SWAP  THEN,  0 # LDA,
    THEN,  PUSH0A JMP,
END-CODE

( Call KERNAL CHRIN function to receive a line of characters from the input )
( channel )
( Requires $CHKIN to have been called )

CODE $READLINE   ( addr count  --  serr )
    2 # LDA,  SETUP JSR,
    BEGIN,  0 # LDA,  INY,
        N CPY,  0= NOT IF,
        CS NOT IF,  SWAP  THEN,
        0FFCF JSR,  0D # CMP,  0= NOT
        IF,  N 2+ )Y STA,  GETST JSR,
        ROT  0= NOT UNTIL,
    ELSE,  DEY,  GETST JSR,
    THEN,
    THEN,
    PHA,  TYA,  0 # LDY,  N 2+ )Y STA,
    PLA,  PUSH0A JMP,
END-CODE

( Call KERNAL CLRCHN function to reset input and output channels )

CODE $CLRCHN   (  --  )
    XSAVE STX,  0FFCC JSR,  XSAVE LDX,  NEXT JMP,
END-CODE

( Call KERNAL SAVE function to write memory region to a device )
( Requires $SETNAM and $SETLFS to have been called )

CODE $SAVE   ( start end  --  serr )
    2 # LDA,  SETUP JSR,  XSAVE STX,
    N LDX,  N 1+ LDY,  N 2+ # LDA,  0FFD8 JSR,
    XSTSERR JMP,
END-CODE

( Call KERNAL LOAD function to read memory region from a device )
( Requires $SETNAM and $SETLFS to have been called )

CODE $LOAD   ( start  --  end serr )
    XSAVE STX,  BOT LDA,  BOT 1+ LDY,  TAX,  0 # LDA,  0FFD5 JSR,
    PHA,  TXA,  XSAVE LDX,  BOT STA,  BOT 1+ STY,
    PLA,  PUSHSERR JMP,
END-CODE

: IODIAG
    ." I/O # "  DUP  0<  IF
	7F00  AND  8  RSHIFT  4B  ELSE
	0FF  AND  53  THEN  EMIT  .  ;

( Test KERNAL result & status and QUIT on any error )

: ?IOERR   ( serr  -- )
    0FFBF  AND  ( mask off EOF ) ?DUP  IF
	[']  IODIAG  FATAL  THEN  ;


( Return a free logical file number )

: NEXTLFN   (  --  lfn )
    0  BEGIN
	0FF  AND  1+  98  C@  0  ?DO   ( loop over all open files )
	    259  I  +  C@  OVER  =  IF
		100  OR  LEAVE  THEN   ( if LFN matches, restart )
	LOOP
    DUP  100  <  UNTIL  ;

( Return a free secondary address for a device )

: NEXTSA   ( da  --  sa )
    1  BEGIN
	0FF  AND  1+  98  C@  0  ?DO   ( loop over all open files )
	    OVER  263  I  +  C@  =  IF   ( if device matches )
		DUP  26D  I  +  C@  0F  AND  =  IF
		    100  OR  LEAVE   ( if secondary matches, restart )
		THEN
	    THEN
	LOOP
    DUP  100  <  UNTIL  NIP  ;


( Allocate and free pages from memory in the region 0x1200 to 0x1FFF )

: GETPAGE   (  --  addr )
    -10  1  10  0  DO
       FREEPGS  @  OVER  AND  ?DUP  IF
           NOT  FREEPGS  @  AND  FREEPGS  !  DROP  I  SWAP  LEAVE
       THEN
       2*  LOOP
    DROP  10  +  8  LSHIFT  ;

: PUTPAGE   ( addr  --  )
    ?DUP  IF
       8  RSHIFT  10  -  1  SWAP  LSHIFT  FREEPGS  @  OR
       FREEPGS  !  THEN  ;


( High-level words resembling ANS Forth File-Access set )

: CLOSE-FILE   ( lfh  --  serr )
    $CLOSE  ;

: OPEN-FILE   ( addr count sa  --  lfh serr )
    >R  $SETNAM  R>
    DEVICE#  @  NEXTLFN  DUP  >R
    $SETLFS
    $OPEN
    R>  SWAP  ;

: READ-FILE   ( addr count1 lfh  --  count2 serr )
    $CHKIN  ?DUP  IF
	>R  DROP  2DROP  0  R>  ELSE
	>R  $READ  R>  $RSTIN
    THEN  ;

: READ-LINE   ( addr count lfh  --  serr )
    $CHKIN  ?DUP  IF
	>R  DROP  2DROP  R>  ELSE
	>R  $READLINE  R>  $RSTIN
    THEN  ;

: WRITE-FILE   ( addr count lfh  --  serr )
    $CHKOUT  ?DUP  IF
	>R  DROP  2DROP  R>  ELSE
	>R  $WRITE  R>  $RSTOUT
    THEN  ;

: WRITE-LINE   ( addr count lfn  --  serr )
    $CHKOUT  ?DUP  IF
	>R  DROP  2DROP  R>  ELSE
	>R  $WRITE  ?DUP  IF
	    DROP  ELSE
	    0D  $WRITEB
	THEN  R>  $RSTOUT
    THEN  ;

: INCLUDED   ( addr count  --  )
    DEVICE#  @  NEXTSA  OPEN-FILE  ?IOERR
    DUP  >R
    >IN  @  INBUF  @   ( Save current source )
    GETPAGE  INBUF  !
    BEGIN
       INBUF  @  0FF  R@  READ-LINE  DUP  ?IOERR
       0  INBUF  @  COUNT  +  C!
       1  >IN  !  >R  INTERPRET  R>
    UNTIL  R>  DROP
    INBUF  @  PUTPAGE
    INBUF  !  >IN  !   ( Restore source )
    CLOSE-FILE  ?IOERR  ;

: SAVED   ( auto addr count  --  serr )
    $SETNAM  1  DEVICE#  @  OVER  $SETLFS
    VOC-LINK  @  ,  FENCE  @  ,  ,
#ifdef ROM_TARGET
    [ AUX.ORIGIN ] LITERAL
#else
    0 +ORIGIN
#endif
    HERE $SAVE  -6  DP  +!  ;

: LOADD   ( addr count  --  serr )
    $SETNAM  1  DEVICE#  @  OVER  $SETLFS
    0  $LOAD  DUP  0<  IF
	NIP
    ELSE
	SWAP  6  -  DUP  DP  !  DUP  @  VOC-LINK  !
	2+  DUP  @  FENCE !  2+  @  ?DUP  IF
	    EXECUTE  THEN
	THEN  ;

( Include the file named by the following word )
: INCLUDE
    NAME  COUNT  INCLUDED  ;

( Conditionally include a file if the first word is not defined )
: INCLUDE?
    NAME  FIND  IF  BL  WORD  2DROP  ELSE  INCLUDE  THEN  ;

( Save the dictionary to the file named by the following word )
: DSAVE
    0  NAME  PAD  OVER  C@  1+  CMOVE>  PAD  COUNT  SAVED  ?IOERR  ;

( Load the dictionary from the file named by the following word )
: DLOAD
    NAME  COUNT  LOADD  ?IOERR  ;

( {mutable} )
' $CLRCHN  EX-ERROR  CATCH
( {immutable} )
