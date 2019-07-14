(                        Through the courtesy of              )

(                         FORTH INTEREST GROUP                )
(                            P.O. BOX  2154                   )
(                         OAKLAND, CALIFORNIA                 )
(                                94621                        )


(                             Release 1.1                     )

(                        with compiler security               )
(                                  and                        )
(                        variable length names                )

(    Further distribution must include the above notice.      )

ASSEMBLER  MEM  HEX
#ifdef ROM_TARGET
0000 ,          ( COLD START VECTOR )
0000 ,          ( WARM START VECTOR )
41 C,  30 C,  0C3 C,  0C2 C,  0CD C,  ( EXROM SIGNATURE )
00 C,           ( PAD )
LABEL RELSRC
0000 ,          ( RELOCATE BLOCK SOURCE )
LABEL RELDST
0000 ,          ( RELOCATE BLOCK DESTINATION )
LABEL RELLEN
0000 ,          ( RELOCATE BLOCK LENGTH )
#else
NOP,   HERE JMP,   ( WORD ALIGNED VECTOR TO COLD )
NOP,   HERE JMP,   ( WORD ALIGNED VECTOR TO WARM )
0004 ,   5ED2 ,  ( CPU, AND REVISION PARAMETERS )
LABEL CLAST
0000 ,           ( TOPMOST WORD IN FORTH VOCABULARY )
0000 ,           ( PAD )
#endif
LABEL COLDIP
0000   ,        ( INITIAL INSTRUCTION POINTER )
LABEL CFENCE
0000   ,        ( INITIAL FENCE )
LABEL COLDDP
0000   ,        ( COLD START VALUE FOR DP )
LABEL CVOCLINK
0000   ,        ( COLD START VALUE FOR VOC-LINK )
LABEL COLDEND

FORTH DEFINITIONS

CODE LIT                   ( PUSH FOLLOWING LITERAL TO STACK *)
     IP )Y LDA,  PHA,  IP INC,  0= IF,  IP 1+ INC,  THEN,
     IP )Y LDA,  LABEL L31  IP INC,  0= IF,  IP 1+ INC,  THEN,
LABEL PUSH      ( PUSH ACCUM AS HI-BYTE, ML STACK AS LO-BYTE *)
     DEX,  DEX,
LABEL PUT          ( REPLACE BOTTOM WITH ACCUM. AND ML STACK *)
     BOT 1+ STA,  PLA,  BOT STA,
LABEL NEXT           ( EXECUTE NEXT FORTH ADDRESS, MOVING IP *)
     1 # LDY,  IP )Y LDA,  W 1+ STA,     ( FETCH CODE ADDRESS )
         DEY,  IP )Y LDA,  W    STA,
     CLC,  IP LDA,  2 # ADC,  IP STA,         ( MOVE IP AHEAD )
     CS IF,  IP 1+ INC,  THEN,
     W 1 - JMP,   ( JUMP INDIR. VIA W THRU CODE FIELD TO CODE )
END-CODE

CODE CLIT
     IP )Y LDA,  PHA,  TYA,  L31  0=  NOT  UNTIL,

LABEL SETUP  ( MOVE # ITEMS FROM STACK TO 'N' AREA OF Z-PAGE *)
     .A ASL,  N 1 - STA,
     BEGIN,  BOT LDA,  N ,Y STA,  INX,  INY,
          N 1 - CPY,  0= UNTIL,  0 # LDY,  RTS,
END-CODE

CODE EXECUTE              ( EXECUTE A WORD BY ITS CODE FIELD *)
                                      ( ADDRESS ON THE STACK *)
   BOT LDA,  W STA,  BOT 1+ LDA,  W 1+ STA,
   INX,  INX,  W 1 - JMP,
END-CODE

CODE BRANCH            ( ADJUST IP BY IN-LINE 16 BIT LITERAL *)
   IP )Y LDA,  PHA,  INY,  IP )Y LDA,  IP 1+ STA,
                           PLA,  IP    STA,  NEXT 2+ JMP,
END-CODE

CODE ?BRANCH           ( IF BOT IS ZERO, BRANCH FROM LITERAL *)
   INX,  INX,  0FE ,X LDA,  0FF ,X ORA,
   'T  BRANCH  0= NOT UNTIL,  ( USE 'BRANCH' FOR FALSE )
 LABEL BUMP                     ( TRUE JUST MOVES IP 2 BYTES *)
   CLC,  IP LDA,  2 # ADC,  IP STA,
   CS IF,  IP 1+ INC, THEN,  NEXT JMP,
END-CODE

CODE LEAVE                       ( IMMEDIATE EXIT OF DO-LOOP *)
   XSAVE STX,  TSX,
   RP) 4 + LDA,  N STA,  RP) 5 + LDA,  N 1+ STA,
   N )Y LDA,  IP STA,  INY,  N )Y LDA,  IP 1+ STA,
 LABEL UNLOOP
   TXA,  CLC,  6 # ADC,  TAX,  TXS,  ( STRIP RETURN STACK )
   XSAVE LDX,  NEXT JMP,
END-CODE

CODE (LOOP)      ( INCREMENT LOOP INDEX, LOOP UNTIL => LIMIT *)
   XSAVE STX,  TSX,  RP) 1+ LDY,
   RP) INC,  0= IF,  RP) 1+ INC,  THEN,
 LABEL LOOPCHK
   TYA,  RP) 1+ EOR,  UNLOOP  0<  NOT UNTIL,
   RP) 4 + LDA,  IP STA,  RP) 5 + LDA,  IP 1+ STA,
   XSAVE LDX,  BUMP JMP,
END-CODE

CODE (+LOOP)          ( INCREMENT INDEX BY STACK VALUE +/-   *)
   BOT LDA,  N STA,  BOT 1+ LDA,  N 1+ STA,  ( STEP )
   INX,  INX,  XSAVE STX,  TSX,  RP) 1+ LDY,
   CLC,  N LDA,  RP) ADC,  RP) STA,
   N 1+ LDA,  RP) 1+ ADC,  RP) 1+ STA,
   LOOPCHK JMP,
END-CODE

CODE (DO)             ( MOVE TWO STACK ITEMS TO RETURN STACK *)
LABEL DODO
   IP  1+ LDA,  PHA,   IP LDA,  PHA, ( LOOP RETURN / PTR TO EXIT )
   SEC 1+ LDA,  PHA,  SEC LDA,  PHA,  ( LIMIT )
   SEC,  BOT LDA,  SEC SBC,  TAY,  BOT 1+ LDA,  SEC 1+ SBC,
   PHA,  TYA,  PHA,  ( INDEX - LIMIT )
   INX,  INX,  INX,  INX,
   BUMP JMP,
END-CODE

CODE (?DO)           ( LOOP UNLESS START AND LIMIT ARE EQUAL *)
   BOT LDA,  SEC EOR,  BOT 1+ EOR,  SEC 1+ EOR,
   DODO  0= UNTIL,  IP )Y LDA,  PHA,  INY,  IP )Y LDA,
   IP 1+ STA,  PLA,  IP STA,

LABEL POPTWO    INX,  INX,
LABEL POP       INX,  INX,  NEXT JMP,
END-CODE

CODE I                    ( COPY CURRENT LOOP INDEX TO STACK *)
   XSAVE STX,  TSX,
   CLC,  RP) LDA,  RP) 2+ ADC,  N STA,
   RP) 1+ LDA,  RP) 3 + ADC,  TAY,
   XSAVE LDX,  N LDA,  PHA,  TYA,  PUSH JMP,
END-CODE

CODE J                 ( COPY NEXT OUTER LOOP INDEX TO STACK *)
   XSAVE STX,  TSX,
   CLC,  RP) 6 + LDA,  RP) 8 + ADC,  N STA,
   RP) 7 + LDA,  RP) 9 + ADC,  TAY,
   XSAVE LDX,  N LDA,  PHA,  TYA,  PUSH JMP,
END-CODE

CODE DIGIT     ( CONVERT ASCII CHAR-SECOND, WITH BASE-BOTTOM *)
                   ( IF OK RETURN DIGIT-SECOND, TRUE-BOTTOM; *)
                                   ( OTHERWISE FALSE-BOTTOM. *)
  SEC,  SEC LDA,   30 # SBC,
  0< NOT IF,  0A # CMP, ( ADJUST FOR ASCII LETTER )
            0< NOT IF,  SEC,  07 # SBC,  0A # CMP,
                       0< NOT IF,
 SWAP ( AT COMPILE TIME )  THEN,  BOT CMP, ( TO BASE )
                      0< IF,  SEC STA,  0FF # LDA,
                      PHA,  PUT JMP,
                      ( STORE RESULT SECOND AND RETURN TRUE )
   THEN,  THEN,  THEN,   ( CONVERSION FAILED )
   TYA,  PHA,  INX,  INX,  PUT JMP,  ( LEAVE BOOLEAN FALSE )
END-CODE

CODE (FIND)  ( CSTR  NFA -- NFA  OR  0 )
     2 # LDA,  SETUP JSR,  XSAVE STX,
BEGIN,  0 # LDY,  N )Y LDA,  N 2+ )Y EOR,  3F # AND,  0=
   IF, ( GOOD ) BEGIN, INY, N )Y LDA,  N 2+ )Y EOR,  .A ASL,  0=
          IF, ( STILL GOOD ) SWAP  CS ( LOOP TILL D7 SET )
     UNTIL,    XSAVE LDX,  DEX,  DEX,  N LDA,  BOT STA,
     N 1+ LDA,  BOT 1+ STA,  NEXT JMP,
     THEN,  THEN, ( MISMATCH ) N 1+ DEC,  0FE # LDY,
    N )Y LDA,  TAX,  INY,  N )Y LDA,  N 1+ STA,  N STX,  N ORA, ( 0 LINK ? )
    0= UNTIL, ( LOOP FOR ANOTHER NAME )
    XSAVE LDX,  0 # LDA, PHA,  PUSH JMP, ( FALSE )
END-CODE

CODE ENCLOSE   ( ENTER WITH ADDRESS-2, DELIM-1.  RETURN WITH *)
   ( ADDR-4, AND OFFSET TO FIRST CH-3, END WORD-2, NEXT CH-1 *)
  2 # LDA,  SETUP JSR,  TXA,  SEC,  8 # SBC,  TAX,
  SEC 1+ STY,  BOT 1+ STY,  ( CLEAR HI BYTES )   DEY,
  BEGIN,  INY,  N 2+ )Y LDA,  ( FETCH CHAR )
     N CMP,  0=  NOT UNTIL,  ( STEP OVER LEADING DELIMITERS )
  BOT 4 + STY,  ( SAVE OFFSET TO FIRST CHAR )
  BEGIN,  N 2+ )Y LDA,  0=
      IF, ( NULL )  SEC STY, ( IN EW )  BOT STY,  ( IN NC )
            TYA,  BOT 4 + CMP,  0=
          IF, ( Y=FC )  SEC INC, ( BUMP EW )  THEN,  NEXT JMP,
       THEN,  SEC STY,  ( IN EW )  INY,  N CMP, ( DELIM ? )
     0= UNTIL,  ( IS DELIM )  BOT STY, ( IN NC )  NEXT JMP,
END-CODE

CODE EMIT             ( PRINT ASCII VALUE ON BOTTOM OF STACK *)
  BOT LDA,  0FFD2 JSR,
  CLC,  10 # LDY,  UP )Y LDA,  01 # ADC,  UP )Y STA,
             INY,  UP )Y LDA,  00 # ADC,  UP )Y STA,  POP JMP,
                                      ( AND INCREMENT 'OUT' )
END-CODE

CODE KEY        ( ACCEPT ONE TERMINAL CHARACTER TO THE STACK *)
  XSAVE STX,  BEGIN,  0FFE4 JSR,  0=  NOT  UNTIL,  XSAVE LDX,
  0FF # CMP,  0=  IF,  7E # LDA,  ELSE,  0C0 # CMP,  CS
  IF,  0E0 # CMP,  CS  IF,  40 # SBC,  ELSE,  SEC,  60 # SBC,  THEN,
  THEN,  THEN,
  LABEL PUSH0A  PHA,  0 # LDA, PUSH JMP,
END-CODE

CODE ?TERMINAL     ( 'BREAK' LEAVES -1 ON STACK; OTHERWISE 0 *)
  XSAVE STX,  0FFE1 JSR,  0=  IF,  DEY,  THEN,
  TYA,  PHA,  XSAVE LDX,  PUSH JMP,
END-CODE

CODE CR         ( EXECUTE CAR. RETURN, LINE FEED ON TERMINAL *)
  0D # LDA,  0FFD2 JSR,  NEXT JMP,
END-CODE

CODE CMOVE   ( WITHIN MEMORY; ENTER W/  FROM-3, TO-2, QUAN-1 *)
  3 # LDA,  SETUP JSR,        ( MOVE 3 ITEMS TO 'N' AREA )
  BEGIN,  BEGIN,  N CPY,  0=  ( DECREMENT BYTE COUNTER AT 'N' )
             IF,  N 1+ DEC,  0<         ( EXIT WHEN DONE )
                  IF,  NEXT JMP,  THEN,  THEN,
         N 4 + )Y LDA,  N 2+ )Y STA,  INY,  0=
      UNTIL,        ( LOOP TILL Y WRAPS, 22 CYCLES/BYTE  )
      N 5 + INC,  N 3 + INC,      ( BUMP HI BYTES OF POINTERS )
   JMP,  ( BACK TO FIRST 'BEGIN' )
END-CODE

CODE CMOVE>  ( WITHIN MEMORY; ENTER W/  FROM-3, TO-2, QUAN-1 *)
  3 # LDA,  SETUP JSR,        ( MOVE 3 ITEMS TO 'N' AREA )
  CLC,  N 1+ LDA,  N 3 + ADC,  N 3 + STA,
  CLC,  N 1+ LDA,  N 5 + ADC,  N 5 + STA,
  N 1+ INC,  N LDY,  CLC,  CS IF,
  BEGIN,  BEGIN,
        DEY,  N 4 + )Y LDA,  N 2+ )Y  STA,
        ROT  THEN,
     TYA,  0= UNTIL,
     N 3 + DEC,  N 5 + DEC,  N 1+ DEC,
  0= UNTIL,
  NEXT JMP,
END-CODE

CODE UM*       ( 16 BIT MULTIPLICAND-2,  16 BIT MULTIPLIER-1 *)
             ( 32 BIT UNSIGNED PRODUCT: LO WORD-2, HI WORD-1 *)
   SEC    LDA,  N    STA,  SEC    STY,
   SEC 1+ LDA,  N 1+ STA,  SEC 1+ STY,  ( MULTIPLICAND TO N )
   10 # LDY,
   BEGIN,  BOT 2+ ASL,  BOT 3 + ROL,  BOT ROL,  BOT 1+ ROL,
             ( DOUBLE PRODUCT WHILE SAMPLING D15 OF MULT   )
         CS IF, ( SET ) CLC,
            ( ADD MULTIPLICAND TO PARTIAL PRODUCT 32 BITS )
               N    LDA,  BOT 2 + ADC,  BOT 2 + STA,
               N 1+ LDA,  BOT 3 + ADC,  BOT 3 + STA,
             CS IF,  BOT INC,  0= IF,  BOT 1+ INC, THEN, THEN,
           THEN,  DEY,  0=    ( CORRECTED FOR CARRY BUG )
      UNTIL,    NEXT JMP,
END-CODE

CODE  UM/MOD     ( 31 BIT DIVIDEND-2, -3,  16 BIT DIVISOR-1  *)
                 ( 16 BIT REMAINDER-2,  16 BIT QUOTIENT-1    *)
  SEC 2 + LDA,  SEC     LDY,  SEC 2 + STY,  .A ASL,  SEC    STA,
  SEC 3 + LDA,  SEC 1+  LDY,  SEC 3 + STY,  .A ROL,  SEC 1+ STA,
  10 # LDA,  N STA,
  BEGIN,  SEC 2 + ROL,  SEC 3 + ROL,  SEC 2+ LDA,
          CS  NOT  IF,
             SEC,  BOT SBC,  TAY,
             SEC 3 + LDA,  BOT 1+ SBC,
             CS IF,  SEC 2+ STY,  BEGIN,  SEC 3 + STA,  SWAP  THEN,
          SEC ROL,  SEC 1+ ROL,
          N DEC,  ROT  0=
      UNTIL,    POP  JMP,
      SWAP  THEN,  BOT SBC,  SEC 2+ STA,  SEC 3 + LDA,  BOT 1+ SBC,  SEC,
      CS  NOT  UNTIL,
END-CODE

CODE 2/
   BOT 1+ LDA,  .A ASL,  ( SIGN BIT INTO CARRY )
   BOT 1+ ROR,  BOT ROR,  NEXT JMP,
END-CODE

CODE 2*
   CLC,  BOT ROL,  BOT 1+ ROL,  NEXT JMP,
END-CODE

CODE AND           ( LOGICAL BITWISE AND OF BOTTOM TWO ITEMS *)
   BOT    LDA,  SEC    AND,  PHA,
   BOT 1+ LDA,  SEC 1+ AND,  INX,  INX,  PUT JMP,
END-CODE

CODE OR           ( LOGICAL BITWISE 'OR' OF BOTTOM TWO ITEMS *)
   BOT    LDA,  SEC     ORA,  PHA,
   BOT 1+ LDA,  SEC 1 + ORA,  INX,  INX,  PUT JMP,
END-CODE

CODE XOR        ( LOGICAL 'EXCLUSIVE-OR' OF BOTTOM TWO ITEMS *)
   BOT    LDA,  SEC    EOR,  PHA,
   BOT 1+ LDA,  SEC 1+ EOR,  INX,  INX,  PUT JMP,
END-CODE

CODE NOT
   0FF # LDA,  TAY,
   BOT EOR,  BOT STA,  TYA,  BOT 1+ EOR,  BOT 1+ STA,
   NEXT JMP,
END-CODE

CODE SP@                      ( FETCH STACK POINTER TO STACK *)
               TXA,  PUSH0A JMP,
END-CODE

CODE SP!                                   ( LOAD INITIAL SP *)
               TOS # LDX,  NEXT JMP,
END-CODE

CODE RP!                                   ( LOAD INITIAL RP *)
    TXA,  0FF # LDX,  TXS,  TAX,  NEXT JMP,

CODE EXIT            ( RESTORE IP REGISTER FROM RETURN STACK *)
   PLA,  IP STA,  PLA,  IP 1+ STA,  NEXT JMP,
END-CODE

CODE >R              ( MOVE FROM COMP. STACK TO RETURN STACK *)
   BOT 1+ LDA,  PHA,  BOT LDA,  PHA,  INX,  INX,  NEXT JMP,
END-CODE

CODE R>              ( MOVE FROM RETURN STACK TO COMP. STACK *)
   DEX,  DEX,  PLA,  BOT STA,  PLA,  BOT 1+ STA,  NEXT JMP,
END-CODE

CODE R@     ( COPY THE BOTTOM OF RETURN STACK TO COMP. STACK *)
   XSAVE STX,  TSX,  RP) LDA,  PHA,  RP) 1+ LDA,
   XSAVE LDX,  PUSH JMP,
END-CODE

CODE 0=           ( REVERSE LOGICAL STATE OF BOTTOM OF STACK *)
   BOT LDA,  BOT 1+ ORA,
   0= IF,  DEY,  THEN,  BOT STY,  BOT 1+ STY,  NEXT JMP,
END-CODE

CODE 0<            ( LEAVE TRUE IF NEGATIVE; OTHERWISE FALSE *)
   BOT 1+ LDA,  0<  IF,  DEY,  THEN,  BOT STY,  BOT 1+ STY,  NEXT JMP,
END-CODE

CODE +         ( LEAVE THE SUM OF THE BOTTOM TWO STACK ITEMS *)
   CLC,  BOT LDA,  SEC ADC,  SEC STA,  BOT 1+ LDA,  SEC 1+ ADC,
         SEC 1+ STA,  INX,  INX,  NEXT JMP,
END-CODE

CODE D+            ( ADD TWO DOUBLE INTEGERS, LEAVING DOUBLE *)
   CLC,  BOT 2 + LDA,  BOT 6 + ADC,  BOT 6 + STA,
         BOT 3 + LDA,  BOT 7 + ADC,  BOT 7 + STA,
         BOT     LDA,  BOT 4 + ADC,  BOT 4 + STA,
         BOT 1 + LDA,  BOT 5 + ADC,  BOT 5 + STA,  POPTWO JMP,
END-CODE

CODE NEGATE        ( TWOS COMPLEMENT OF BOTTOM SINGLE NUMBER *)
   SEC,  LABEL NEGATEP  TYA,  BOT    SBC,  BOT    STA,
         TYA,  BOT 1+ SBC,  BOT 1+ STA,  NEXT JMP,
END-CODE

CODE DNEGATE       ( TWOS COMPLEMENT OF BOTTOM DOUBLE NUMBER *)
   SEC,  TYA,  BOT 2 + SBC,  BOT 2 + STA,
         TYA,  BOT 3 + SBC,  BOT 3 + STA,
      NEGATEP JMP,
END-CODE

CODE OVER              ( DUPLICATE SECOND ITEM AS NEW BOTTOM *)
   SEC LDA,  PHA,  SEC 1+ LDA,  PUSH JMP,
END-CODE

CODE DROP                           ( DROP BOTTOM STACK ITEM *)
   POP  -2  BYTE.IN  DROP  ! ( C.F. VECTORS DIRECTLY TO 'POP' )
END-CODE

CODE SWAP        ( EXCHANGE BOTTOM AND SECOND ITEMS ON STACK *)
   SEC LDA,  PHA,  BOT LDA,  SEC STA,
   SEC 1+ LDA,  BOT 1+ LDY,  SEC 1+ STY,  PUT JMP,
END-CODE

CODE DUP                    ( DUPLICATE BOTTOM ITEM ON STACK *)
   BOT LDA,  PHA,  BOT 1+ LDA,  PUSH JMP,
END-CODE

CODE +!   ( ADD SECOND TO MEMORY 16 BITS ADDRESSED BY BOTTOM *)
   CLC,  BOT X) LDA,  SEC ADC,  BOT X) STA,
   BOT INC,  0= IF,  BOT 1+ INC,  THEN,
   BOT X) LDA,  SEC 1+ ADC,  BOT X) STA,  POPTWO JMP,
END-CODE

CODE TOGGLE          ( BYTE AT ADDRESS-2, BIT PATTERN-1  ... *)
       SEC X) LDA,  BOT EOR,  SEC X) STA,  POPTWO JMP,
END-CODE

CODE @                   ( REPLACE STACK ADDRESS WITH 16 BIT *)
    BOT X) LDA,  PHA,             ( CONTENTS OF THAT ADDRESS *)
    BOT INC,  0= IF, BOT 1+ INC,  THEN,  BOT X) LDA,  PUT JMP,
END-CODE

CODE C@      ( REPLACE STACK ADDRESS WITH POINTED 8 BIT BYTE *)
    BOT X) LDA,  BOT STA,  BOT 1+ STY,  NEXT JMP,
END-CODE

CODE !         ( STORE SECOND AT 16 BITS ADDRESSED BY BOTTOM *)
    SEC LDA,  BOT X) STA,  BOT INC,  0= IF,  BOT 1+ INC,  THEN,
    SEC 1+ LDA,  BOT X) STA,  POPTWO JMP,
END-CODE

CODE C!           ( STORE SECOND AT BYTE ADDRESSED BY BOTTOM *)
    SEC LDA,  BOT X) STA,  POPTWO JMP,
END-CODE

: :                 ( CREATE NEW COLON-DEFINITION UNTIL ';' *)
                   ?EXEC !CSP LAST         CONTEXT    !
               CREATE  SMUDGE  ]     ;CODE   IMMEDIATE
     IP 1+ LDA,  PHA,  IP LDA,  PHA,  CLC,  W LDA,  2 # ADC,
     IP STA,  TYA,  W 1+ ADC,  IP 1+ STA,  NEXT JMP,
END-CODE

: ;                             ( TERMINATE COLON-DEFINITION *)
                    ?CSP  COMPILE     EXIT
                  SMUDGE  [COMPILE] [    ;   IMMEDIATE

: CONSTANT              ( WORD WHICH LATER CREATES CONSTANTS *)
                      CREATE  ,     ;CODE
       2 # LDY,  W )Y LDA,  PHA,  INY,  W )Y LDA,  PUSH JMP,
END-CODE

: VARIABLE              ( WORD WHICH LATER CREATES VARIABLES *)
     CREATE  2  ALLOT  ;

: USER                                ( CREATE USER VARIABLE *)
     CONSTANT  ;CODE
       2 # LDY,  CLC,  W )Y LDA,  UP ADC,  PHA,
       0 # LDA,  UP 1+ ADC,  PUSH JMP,
END-CODE

HEX
-1  CONSTANT  -1
00  CONSTANT  0       01  CONSTANT  1
02  CONSTANT  2       03  CONSTANT  3
20  CONSTANT  BL                               ( ASCII BLANK *)
16  CONSTANT  C/L                 ( TEXT CHARACTERS PER LINE *)

           00  +ORIGIN
: +ORIGIN  LITERAL  +  ; ( LEAVES ADDRESS RELATIVE TO ORIGIN *)

HEX
00   USER  FENCE                    ( BARRIER FOR FORGETTING *)
02   USER  DP                           ( DICTIONARY POINTER *)
04   USER  VOC-LINK                   ( TO NEWEST VOCABULARY *)
06   USER  CSP                        ( CHECK STACK POSITION *)
08   USER  HLD        ( POINTS TO LAST CHARACTER HELD IN PAD *)
0A   USER  FREEPGS                    ( BITMAP OF FREE PAGES *)
0C   USER  INBUF                     ( INTERPRETATION BUFFER *)
0E   USER  >IN                     ( OFFSET INTO SOURCE TEXT *)
10   USER  OUT                     ( DISPLAY CURSOR POSITION *)
12   USER  SPAN              ( COUNT OF CHARS RCVD BY EXPECT *)
14   USER  #TIB                               ( BYTES IN TIB *)
16   USER  CONTEXT               ( VOCABULARY FIRST SEARCHED *)
18   USER  CURRENT          ( SEARCHED SECOND, COMPILED INTO *)
1A   USER  STATE                         ( COMPILATION STATE *)
1C   USER  BASE                   ( FOR NUMERIC INPUT-OUTPUT *)
1E   USER  DEVICE#               ( CURRENT BUS DEVICE NUMBER *)
20   USER  DPL                      ( DECIMAL POINT LOCATION *)

CODE 1+                      ( INCREMENT STACK NUMBER BY ONE *)
    BOT INC,  0= IF,  BOT 1+ INC, THEN, NEXT JMP,
END-CODE
: 2+      2   +  ;           ( INCREMENT STACK NUMBER BY TWO *)
CODE 1-                      ( DECREMENT STACK NUMBER BY ONE *)
    BOT LDA,  BOT DEC,  TAY,  0= IF,
        BOT 1+ DEC,  THEN,  NEXT JMP,
END-CODE
: 2-      2   -  ;           ( DECREMENT STACK NUMBER BY TWO *)
: HERE    DP  @  ;        ( FETCH NEXT FREE ADDRESS IN DICT. *)
: ALLOT   DP  +! ;                ( MOVE DICT. POINTER AHEAD *)
: ,   HERE  !  2  ALLOT  ;     ( ENTER STACK NUMBER TO DICT. *)
: C,   HERE  C!  1   ALLOT  ;    ( ENTER STACK BYTE TO DICT. *)
: -   NEGATE  +  ;                ( LEAVE DIFF. SEC - BOTTOM *)
: =   -  0=  ;                   ( LEAVE BOOLEAN OF EQUALITY *)
: 0>   NEGATE  0<  ;

CODE <                          ( LEAVE BOOLEAN OF SEC < BOT *)
   SEC,  SEC LDA,  BOT SBC,  SEC 1+ LDA,
   BOT 1+ SBC,  VS  IF,  80 # EOR,  THEN,
   0<  IF,  DEY,  THEN,  SEC STY,  SEC 1+ STY,  POP JMP,
END-CODE

: >   SWAP  <  ;                ( LEAVE BOOLEAN OF SEC > BOT *)
: ROT   >R  SWAP  R>  SWAP  ;       ( ROTATE THIRD TO BOTTOM *)
: SPACE     BL  EMIT  ;            ( PRINT BLANK ON TERMINAL *)
: ?DUP     DUP  IF  DUP  THEN  ;        ( DUPLICATE NON-ZERO *)

: PICK
   1+  2*  SP@  +  @  ;

: ROLL
   DUP  >R  PICK  SP@  DUP  2+  R>  1+  2*  CMOVE>  DROP  ;

CODE U<
   SEC LDA,  BOT CMP,  SEC 1+ LDA,
   BOT 1+ SBC,  TYA,
   0 # SBC,  SEC STA,  SEC 1+ STA,  POP JMP,
END-CODE

: 2DUP   OVER  OVER  ;
: 2DROP   DROP  DROP  ;

: DEPTH   SP@  ASSEMBLER  TOS  SWAP  -  2/  ;

: D<
   ROT  2DUP  -
      IF  >  NIP  NIP  ELSE  2DROP  U<  THEN  ;

: TRAVERSE                          ( MOVE ACROSS NAME FIELD *)
         ( ADDRESS-2, DIRECTION-1, I.E. -1=R TO L, +1=L TO R *)
       SWAP
       BEGIN  OVER  +  7F  OVER  C@  <  UNTIL  NIP  ;

: LAST         CURRENT  @  ;      ( ADDR OF NFA OF LAST WORD *)

( FOLLOWING HAVE LITERALS DEPENDENT ON COMPUTER WORD SIZE )

: >BODY  2+  ;                  ( CONVERT A WORDS CFA TO PFA *)
: BODY>  2-  ;                  ( CONVERT A WORDS PFA to CFA *)
: >NAME  1-  -1  TRAVERSE  ;    ( CONVERT A WORDS CFA TO NFA *)
: NAME>  1  TRAVERSE  1+  ;     ( CONVERT A WORDS NFA TO CFA *)
: >LINK  >NAME  2-  ;           ( CONVERT A WORDS CFA TO LFA *)

: !CSP     SP@  CSP  !  ;     ( SAVE STACK POSITION IN 'CSP' *)

: ?ERROR          ( BOOLEAN-2,  ERROR TYPE-1,  WARN FOR TRUE *)
         SWAP  IF         ERROR    ELSE  DROP  THEN  ;

: ?COMP   STATE @  0= 11 ?ERROR ;   ( ERROR IF NOT COMPILING *)

: ?EXEC   STATE  @  12  ?ERROR  ;   ( ERROR IF NOT EXECUTING *)

: ?PAIRS  -  13  ?ERROR  ;  ( VERIFY STACK VALUES ARE PAIRED *)

: ?CSP   SP@  CSP @ -  14  ?ERROR  ; ( VERIFY STACK POSITION *)

: COMPILE          ( COMPILE THE EXECUTION ADDRESS FOLLOWING *)
        ?COMP  R>  DUP  2+  >R  @  ,  ;

: [    0  STATE  !  ;  IMMEDIATE          ( STOP COMPILATION *)

: ]    0C0  STATE  !  ;            ( ENTER COMPILATION STATE *)

: SMUDGE    LAST  @  20  TOGGLE  ;    ( ALTER LAST WORD NAME *)

: HEX      10  BASE  !  ;         ( MAKE HEX THE IN-OUT BASE *)

: DECIMAL  0A  BASE  !  ;     ( MAKE DECIMAL THE IN-OUT BASE *)

: (;CODE)     ( WRITE CODE FIELD POINTING TO CALLING ADDRESS *)
        R>  LAST  @  NAME>  !  ;

: ;CODE                      ( TERMINATE A NEW DEFINING WORD *)
      ?CSP  COMPILE  (;CODE)
      [COMPILE]  [  SMUDGE  ;   IMMEDIATE

CODE DODOES
     IP 1+ LDA,  PHA,  IP LDA,  PHA,  ( BEGIN FORTH NESTING )
     W )Y LDA,  N STA,  INY,  W )Y LDA,  N 1+ STA,
     CLC,  N LDA,  3 # ADC,  IP STA,
     N 1+ LDA,  0 # ADC,  IP 1+ STA,
LABEL DOCREATE
     CLC,  W LDA,  2 # ADC,  PHA,  W 1+ LDA,  0 # ADC,
     PUSH JMP,
END-CODE

: DOES>          ( REWRITE PFA WITH CALLING HI-LEVEL ADDRESS *)
                             ( REWRITE CFA WITH 'DOES>' CODE *)
     COMPILE  (;CODE)  4C  C,  ASSEMBLER 'T DODOES  LITERAL  ,  ;
                                                  IMMEDIATE

: COUNT    DUP 1+ SWAP C@  ;  ( LEAVE TEXT ADDR. CHAR. COUNT *)
: TYPE            ( TYPE STRING FROM ADDRESS-2, CHAR.COUNT-1 *)
        ?DUP  IF OVER + SWAP
                 DO I C@ EMIT LOOP  ELSE DROP THEN ;
: -TRAILING   ( ADJUST CHAR. COUNT TO DROP TRAILING BLANKS *)
        DUP  0  DO  2DUP  +  1-  C@
        BL  -  IF  LEAVE  ELSE  1-  THEN  LOOP  ;
: (.")             ( TYPE IN-LINE STRING, ADJUSTING RETURN *)
        R@  COUNT  DUP  1+  R>  +  >R  TYPE  ;

: ."                               ( COMPILE QUOTED STRING *)
        COMPILE  (.")  22  WORD  C@  1+  ALLOT  ;
               IMMEDIATE

: .(                                 ( PRINT QUOTED STRING *)
        29  WORD  COUNT  TYPE  ;  IMMEDIATE

CODE (EXPECT)  ( ADDR MAX -- LEN )
    SEC LDA,  N STA,  SEC 1+ LDA,  N 1+ STA,
    BOT LDA,  N 2+ STA,  XSAVE STX,  24 C,  ( SKIP FIRST INY )
    BEGIN,  INY,  N 2+ CPY,
       CS NOT IF,  0FFCF JSR,
       0FF # CMP,  0=  IF,  7E # LDA,  ELSE,  0C0 # CMP,
       CS  IF,  0E0 # CMP,  CS  IF,  40 # SBC,  ELSE,
       SEC,  60 # SBC,  THEN,  THEN,  THEN,
       N )Y STA,  0D # CMP,  SWAP
    0= UNTIL,
    THEN,  0  # LDA,  N )Y STA,  XSAVE LDX,
    SEC STY,  SEC 1+ STA,  POP JMP,
END-CODE

: EXPECT           ( TERMINAL INPUT MEMORY-2,  CHAR LIMIT-1 *)
   (EXPECT)  SPAN  !  ;

: TIB       ASSEMBLER  TIBX  ;

: QUERY     TIB  58  EXPECT  0  >IN  !  SPAN  @  #TIB  !  ;

8081  HERE  2+
: X   R>  DROP  ;   !   IMMEDIATE      ( END-OF-TEXT IS NULL *)

: FILL               ( FILL MEMORY BEGIN-3,  QUAN-2,  BYTE-1 *)
        SWAP  >R  OVER  C!  DUP  1+  R>  1-  CMOVE  ;

: ERASE           ( FILL MEMORY WITH ZEROS  BEGIN-2,  QUAN-1 *)
        0  FILL  ;

: BLANK                   ( FILL WITH BLANKS BEGIN-2, QUAN-1 *)
        BL  FILL  ;

: HOLD                               ( HOLD CHARACTER IN PAD *)
        -1  HLD  +!   HLD  @  C!  ;

: PAD        HERE  44  +  ;     ( PAD IS 68 BYTES ABOVE HERE *)
        ( DOWNWARD HAS NUMERIC OUTPUTS; UPWARD MAY HOLD TEXT *)

: WORD         ( ENTER WITH DELIMITER, MOVE STRING TO 'HERE' *)
   INBUF  @
   >IN  @  +  SWAP   ( ADDRESS-2, DELIMITER-1 )
   ENCLOSE         ( ADDRESS-4, START-3, END-2, TOTAL COUNT-1 )
   HERE  22  BLANK       ( PREPARE FIELD OF 34 BLANKS )
   >IN  +!         ( STEP OVER THIS STRING )
   OVER  -  >R     ( SAVE CHAR COUNT )
   R@  HERE  C!    ( LENGTH STORED FIRST )
   +  HERE  1+
   R>  CMOVE       ( MOVE STRING FROM BUFFER TO HERE+1 )
   HERE  ;

: CONVERT     ( CONVERT DOUBLE NUMBER, LEAVING UNCONV. ADDR. *)
   BEGIN  1+  DUP  >R  C@  BASE  @  DIGIT
      WHILE  SWAP  BASE  @  UM*  DROP  ROT  BASE  @  UM*  D+
      DPL  @  1+  IF  1  DPL  +!  THEN  R>  REPEAT  R>  ;

: NUMBER   ( ENTER W/ STRING ADDR.  LEAVE DOUBLE NUMBER *)
      0  0  ROT  DUP  1+  C@  2D  =  DUP  >R  IF  1+  THEN  -1
   BEGIN  DPL  !  CONVERT  DUP  C@  BL  -
      WHILE  DUP  C@  2E  -  0  ?ERROR    0  REPEAT
      DROP  R>  IF  DNEGATE  THEN  ;

: NAME   ( -- CSTR )
      BL WORD
      DUP  C@  1+  OVER  +  OVER  1+  DO   ( STRIP TOP BIT )
         I  C@  7F  AND  I  C!  LOOP  ;

: FIND   ( CSTR -- CFA  -1/1  OR  0 )
      DUP  CONTEXT  @  @  (FIND)
      DUP  0=  IF  DROP  LAST  @  (FIND)
               ELSE  NIP  THEN
      DUP  IF
         DUP  NAME>  SWAP  C@  40  AND  IF  1  ELSE  -1  THEN
      THEN ;

: ID.   ( PRINT NAME FIELD FROM ITS HEADER ADDRESS *)
     1+  BEGIN
        DUP  1+  SWAP  C@  DUP  7F  AND  EMIT  80  AND
     UNTIL  DROP  SPACE  ;

: CREATE                      ( A CODE HEADER TO PARAM FIELD *)
                     ( WARNING IF DUPLICATING A CURRENT NAME *)
      FENCE  HERE  0A0  +  <  2  ?ERROR  ( 6502 only )
      LAST  @  ,
      NAME  FIND    ( CHECK IF UNIQUE IN CURRENT AND CONTEXT )
      IF ( WARN USER )  >NAME  ID.
                        4         MESSAGE    SPACE  THEN
      HERE  DUP  C@  1F    MIN    1+  ALLOT
      DP  C@  0FF  =  IF  20  C,  THEN
      DUP  80  TOGGLE  HERE  1-  80  TOGGLE ( DELIMIT BITS )
      LAST  !
      HERE  2+  ,  ;CODE  DOCREATE JMP,
END-CODE

: EXCEPTION   ( CFA -- )
    CREATE  ,  0  ,  ;

: CATCH   ( CFA EXCEPT  --  )
    HERE  >R  >R
    R@  @  ,  R@  2+  @  ,   ( COPY PREVIOUS ENTRY )
    R@  !   ( SAVE THIS CFA )
    R>  R>  SWAP  2+  !   ( LINK TO NEW ENTRY )
;

: RAISE   ( EXCEPT  --  )
    BEGIN
	DUP  >R  @  EXECUTE  R>
	2+  @  ?DUP
    0= UNTIL ;

: WHERE
    HERE  COUNT  TYPE  ;

( {mutable} )
' WHERE  EXCEPTION  EX-ERROR
( {immutable} )

: FATAL
    EX-ERROR  RAISE  ."  ? "  EXECUTE  SP!  2DROP  0  0  QUIT  ;

: MESSAGE
    BASE  @  SWAP  DECIMAL  ." MSG # "  .  BASE  !  ;

: ERROR
    [']  MESSAGE  FATAL  ;

: [COMPILE]         ( FORCE COMPILATION OF AN IMMEDIATE WORD *)
      '  ,  ;             IMMEDIATE

: LITERAL                     ( IF COMPILING, CREATE LITERAL *)
      STATE  @  IF  DUP  0>  OVER  100  <  AND  IF
          COMPILE  CLIT  C,  ELSE
	  COMPILE  LIT  ,  THEN  THEN  ;  IMMEDIATE

: DLITERAL             ( IF COMPILING, CREATE DOUBLE LITERAL *)
      STATE  @  IF  SWAP  [COMPILE]  LITERAL
                          [COMPILE]  LITERAL  THEN ; IMMEDIATE

(  FOLLOWING DEFINITION IS INSTALLATION DEPENDENT )
: ?STACK    ( QUESTION UPON OVER OR UNDERFLOW OF STACK *)
      ASSEMBLER
      SP@  TOS  SWAP  <  1  ?ERROR   SP@  BOS  <  7  ?ERROR  ;

: INTERPRET   ( INTERPRET OR COMPILE SOURCE TEXT INPUT WORDS *)
      BEGIN  NAME  DUP  FIND  ?DUP
         IF  ( FOUND )  STATE  @  *  0<  ROT  DROP
                IF  ,  ELSE  EXECUTE  THEN  ?STACK
            ELSE  NUMBER  DPL  @  1+
                IF  [COMPILE]  DLITERAL
                  ELSE   DROP  [COMPILE]  LITERAL  THEN  ?STACK
          THEN  AGAIN  ;

: IMMEDIATE          ( TOGGLE PREC. BIT OF LAST CURRENT WORD *)
         LAST  @  40  TOGGLE  ;

: VOCABULARY  ( CREATE VOCAB WITH 'V-HEAD' AT VOC INTERSECT. *)
       CREATE  LAST  2+  ,  0A081  ,
       HERE  VOC-LINK  @  ,  VOC-LINK  !
       DOES>  CONTEXT  !  ;

( {mutable} )
VOCABULARY  FORTH     IMMEDIATE       ( THE TRUNK VOCABULARY *)
( {immutable} )

: DEFINITIONS        ( SET THE CONTEXT ALSO AS CURRENT VOCAB *)
       CONTEXT  @  CURRENT  !  ;

: (              ( SKIP INPUT TEXT UNTIL RIGHT PARENTHESIS *)
       29  WORD  DROP  ;   IMMEDIATE

: QUIT                   ( RESTART,  INTERPRET FROM TERMINAL *)
      0FFC  FREEPGS  !
      TIB  INBUF  !  [COMPILE]  [
      BEGIN  RP!  CR  QUERY  INTERPRET
             STATE  @  0=  IF  ."  OK"  THEN  AGAIN  ;

: ABORT                  ( WARM RESTART, INCLUDING REGISTERS *)
      SP!  DECIMAL  8  DEVICE#  !
      93  EMIT  ." **** V-FORTH  3.8 ****"
      [COMPILE]  FORTH  DEFINITIONS  QUIT  ;

CODE COLD               ( COLD START, INITIALIZING USER AREA *)
#ifdef ROM_TARGET
   6 # LDY,           ( COPY RELOCATE PARAMETERS TO ZERO PAGE )
   BEGIN,
     RELSRC 1- ,Y LDX,
     N 1- ,Y STX,
     DEY,
   0= UNTIL,
   BEGIN,   ( COPY RELOC BLOCK TO RAM )
     N )Y LDA,  N 2+ )Y STA,
     INY,
     N 4 + CPY,  0= UNTIL,
#else
     CLAST    LDA,  'T FORTH    STA,  ( FORTH VOCAB. )
     CLAST 1+ LDA,  'T FORTH 1+ STA,
#endif
      COLDEND CFENCE - 1- # LDY, ( INDEX TO VOC-LINK )
      0C0 # LDA, UP STA,  ( LOAD UP FROM MEMHIGH )
      284 LDX,  DEX,  UP 1+ STX,
       BEGIN,  CFENCE ,Y LDA,  ( FROM LITERAL AREA )
                       UP )Y STA,  ( TO USER AREA )
            DEY,  0< UNTIL,
     INY,
LABEL ZBRK
     00 STY,
     COLDIP 1+ LDA,  IP 1+ STA,
     COLDIP    LDA,  IP    STA,
      6C # LDA,  W 1 - STA,    'T RP! JMP, ( RUN )

( SYSTEM WARM START )
#ifndef ROM_TARGET
LABEL NMIVEC
    PHA,  TXA,  PHA,  TYA,  PHA,  911D LDA,
    0< IF,  911E AND,  TAX,  02 # AND,
    0= IF,  0FEDE JMP,  THEN,
#else
    HERE  02 +ORIGIN !   ( POINT RE-ENTRY TO HERE )
#endif
    9111 BIT,  0F734 JSR,  0FFE1 JSR,  0= NOT IF,
#ifndef ROM_TARGET
    SWAP  THEN,
#endif
    0FEFF JMP,  THEN,
#ifndef ROM_TARGET
    HERE  06 +ORIGIN !   ( POINT RE-ENTRY TO HERE )
#endif

LABEL BRKVEC
    0FDF9 JSR,  0E518 JSR,
    00 # LDY,
    CLI,  ZBRK JMP,

( SYSTEM COLD START )
#ifdef ROM_TARGET
   HERE  00  +ORIGIN  !  ( POINT COLD ENTRY TO HERE )
   0FD8D JSR,         ( INITIALIZE KERNAL AS NORMAL )
   0FD52 JSR,
   0FDF9 JSR,
   0E518 JSR,
#else
   HERE  02  +ORIGIN  !  ( POINT COLD ENTRY TO HERE )
   NMIVEC  100  /MOD  # LDA,  319 STA,  # LDA,  318 STA,
#endif
   BRKVEC  100  /MOD  # LDA,  317 STA,  # LDA,  316 STA,
   CLI,
   'T COLD JMP,
END-CODE

: (ABORT")
      R>  DUP  COUNT  +  >R  SWAP  IF  COUNT  TYPE  ABORT  THEN  DROP  ;

: ABORT"
      COMPILE  (ABORT")  22  WORD  C@  1+  ALLOT  ;
               IMMEDIATE

CODE S->D                  ( EXTEND SINGLE INTEGER TO DOUBLE *)
      BOT 1+ LDA,  0< IF, DEY, THEN,  TYA, PHA, PUSH JMP,
END-CODE

: +-    0< IF NEGATE THEN ;   ( APPLY SIGN TO NUMBER BENEATH *)

: D+-                  ( APPLY SIGN TO DOUBLE NUMBER BENEATH *)
        0<  IF  DNEGATE  THEN  ;

: ABS     DUP  +-   ;                 ( LEAVE ABSOLUTE VALUE *)
: DABS    DUP  D+-  ;        ( DOUBLE INTEGER ABSOLUTE VALUE *)

: MIN                         ( LEAVE SMALLER OF TWO NUMBERS *)
        2DUP  >  IF  SWAP  THEN  DROP  ;
: MAX                          ( LEAVE LARGER OF TWO NUMBERS *)
        2DUP  <  IF  SWAP  THEN  DROP  ;

: M*     ( LEAVE SIGNED DOUBLE PRODUCT OF TWO SINGLE NUMBERS *)
        2DUP  XOR  >R  ABS  SWAP  ABS  UM*  R>  D+-  ;
: M/MOD           ( FROM SIGNED DOUBLE-3-2, SIGNED DIVISOR-1 *)
               ( LEAVE SIGNED REMAINDER-2, SIGNED QUOTIENT-1 *)
        DUP  >R  ABS  OVER
            0<  IF  TUCK  +  SWAP  THEN
        UM/MOD  R@
        0<  IF  NEGATE  OVER  IF  SWAP  R@  +  SWAP  1-
        THEN THEN  R>  DROP  ;
: *      UM*  DROP  ;                       ( SIGNED PRODUCT *)
: /MOD   >R  S->D  R>  M/MOD  ;        ( LEAVE REM-2, QUOT-1 *)
: /      /MOD  NIP  ;                       ( LEAVE QUOTIENT *)
: MOD    /MOD  DROP  ;                     ( LEAVE REMAINDER *)
: */MOD              ( TAKE RATION OF THREE NUMBERS, LEAVING *)
         >R  M*  R>  M/MOD  ;            ( REM-2, QUOTIENT-1 *)
: */     */MOD  NIP  ;          ( LEAVE RATIO OF THREE NUMBS *)
: UD/MOD   ( DOUBLE, SINGLE DIVISOR ...  REMAINDER, DOUBLE *)
          >R  0  R@  UM/MOD  R>  SWAP  >R  UM/MOD  R>   ;

: '                                    ( FIND NEXT WORDS CFA *)
    NAME  FIND  0=  0  ?ERROR  ;

: [']                      ( FIND NEXT WORDS CFA; COMPILE IT *)
      '  [COMPILE]  LITERAL  ;  IMMEDIATE

HEX
: FORGET                      ( Dave Kilbridge's Smart Forget )
     [COMPILE]  '  >NAME  DUP  FENCE  @  U<  15  ?ERROR
#ifdef ROM_TARGET
     0  +ORIGIN  OVER  U<  15  ?ERROR
#endif
     >R  VOC-LINK  @  ( start with latest vocabulary )
   BEGIN  R@  OVER  U<  WHILE  [COMPILE]  FORTH  DEFINITIONS
       @  DUP  VOC-LINK  !  REPEAT  ( unlink from voc list )
   BEGIN  DUP  2-     ( start with phantom nfa )
       BEGIN  NAME>  >LINK  @  DUP  R@  U<
#ifdef ROM_TARGET
       0  +ORIGIN  2  PICK  U<  OR
#endif
       UNTIL
       OVER  4 -  !  @  ?DUP  0=  UNTIL ( end of list ? )
     R>  2-  DP  !  ;

: BEGIN    ?COMP  HERE  1  ;                  IMMEDIATE

: THEN     ?COMP 2 ?PAIRS  HERE  SWAP  !  ;  IMMEDIATE

: DO       COMPILE  (DO)  HERE  0  ,  3  ;            IMMEDIATE

: ?DO      COMPILE  (?DO)  HERE  0  ,  3  ;           IMMEDIATE

: LOOP     3  ?PAIRS  COMPILE  (LOOP)  HERE  SWAP  !  ;  IMMEDIATE

: +LOOP    3  ?PAIRS  COMPILE  (+LOOP)  HERE  SWAP  !  ;  IMMEDIATE

: UNTIL    1  ?PAIRS  COMPILE  ?BRANCH  ,  ; IMMEDIATE

: AGAIN    1  ?PAIRS  COMPILE  BRANCH  ,  ;   IMMEDIATE

: REPEAT   >R  >R  [COMPILE]  AGAIN
              R>  R>  2-  [COMPILE]  THEN  ;  IMMEDIATE

: IF       COMPILE  ?BRANCH   HERE  0  ,  2  ;  IMMEDIATE

: ELSE     2  ?PAIRS  COMPILE  BRANCH  HERE  0  ,
           SWAP  2  [COMPILE]  THEN  2  ;      IMMEDIATE

: WHILE   [COMPILE]  IF  2+  ;    IMMEDIATE

: SPACES     0  MAX  0  ?DO  SPACE  LOOP  ;

: <#     PAD  HLD  !  ;

: #>     2DROP  HLD  @  PAD  OVER  -  ;

: SIGN   0<  IF  2D  HOLD  THEN  ;

: #                     ( CONVERT ONE DIGIT, HOLDING IN PAD * )
         BASE @ UD/MOD ROT 9 OVER < IF  7 + THEN 30  +  HOLD  ;

: #S     BEGIN  #  2DUP  OR  0=  UNTIL  ;

: D.R        ( DOUBLE INTEGER OUTPUT, RIGHT ALIGNED IN FIELD *)
       >R  TUCK  DABS  <#  #S  ROT  SIGN  #>
       R>  OVER  -  SPACES  TYPE  ;

: D.     0  D.R  SPACE  ;            ( DOUBLE INTEGER OUTPUT *)

: .R     >R  S->D  R>  D.R  ;       ( ALIGNED SINGLE INTEGER *)

: .      S->D  D.  ;                 ( SINGLE INTEGER OUTPUT *)

: U.     0  D.  ;                  ( UNSIGNED INTEGER OUTPUT *)

: ?      @  .  ;                  ( PRINT CONTENTS OF MEMORY *)

: WORDS                            ( LIST CONTEXT VOCABULARY *)
               C/L  OUT  !    CONTEXT  @  @
     BEGIN  OUT  @  C/L  >  IF  CR  0  OUT  !  THEN
            DUP  ID.  SPACE  NAME>  >LINK  @
            DUP  0=  ?TERMINAL  OR  UNTIL  DROP  ;

HEX

: FORTH-83   ( NOP )  ;


( ANS Forth definitions )

: FALSE   0  ;
: TRUE   -1  ;

CODE LSHIFT   ( x1 u  --  x2 )
    BEGIN,  TYA,  BOT CMP,  CS NOT IF,  SEC ASL,  SEC 1+ ROL,
    INY,  SWAP  JMP,  THEN,  POP JMP,
END-CODE

CODE RSHIFT   ( x1 u  --  x2 )
    BEGIN,  TYA,  BOT CMP,  CS NOT IF,  SEC 1+ LSR,  SEC ROR,
    INY,  SWAP  JMP,  THEN,  POP JMP,
END-CODE

: (")   R>  DUP  COUNT  +  >R  ;

: S"     22  STATE  @      ( COMPILE OR STORE QUOTED STRING *)
    IF  COMPILE  (")  WORD  C@  1+  ALLOT  COMPILE  COUNT
    ELSE  WORD  PAD  OVER  C@  1+  CMOVE  PAD  COUNT  THEN  ;
            IMMEDIATE

: CHAR   (  --  char )
    BL  WORD  1+  C@  ;

: [CHAR]
    BL  WORD  1+  C@  [COMPILE]  LITERAL  ;    IMMEDIATE

: NIP   ( x1 x2  --  x2 )
    SWAP  DROP  ;
: TUCK   ( x1 x2  --  x2 x1 x2 )
    SWAP  OVER  ;

CODE D2/
   BOT 1+ LDA,  .A ASL,
   BOT 1+ ROR,  BOT ROR,  SEC 1+ ROR,  SEC ROR,  NEXT JMP,
END-CODE

: D0=   OR  0=  ;
: D-   DNEGATE  D+  ;
: D=   D-  D0=  ;
: 2OVER   3  PICK  3  PICK  ;
: 2SWAP   3  ROLL  3  ROLL  ;
: 2ROT   5  ROLL  5  ROLL  ;
: D>   2SWAP  D<  ;
: DMIN   2OVER  2OVER  D>  IF  2SWAP  THEN  2DROP  ;
: 2!   ROT  OVER  !  2+  !  ;
: 2@   DUP  @  SWAP  2+  @  ;
: DU<   ROT  SWAP  U<  >R  U<  R>  OR  ;

: 2VARIABLE
   CREATE  4  ALLOT  ;

: 2CONSTANT
    CREATE  SWAP  ,  ,  DOES>  2@  ;

#include "system.fs"

ASSEMBLER
#ifdef ROM_TARGET
HERE                  RELSRC  !  ( RELOCATE BLOCK SOURCE )
AUX.ORIGIN            RELDST  !  ( RELOCATE BLOCK DESTINATION )
AUX.HERE AUX.ORIGIN - RELLEN  !  ( RELOCATE BLOCK LENGTH )
AUX.HERE   CFENCE  !        ( COLD START FENCE )
AUX.HERE   COLDDP  !        ( COLD START DP )
#else
HERE       CFENCE  !        ( COLD START FENCE )
HERE       COLDDP  !        ( COLD START DP )
LATEST     CLAST  !         ( TOPMOST WORD )
#endif
'  ABORT  2+    COLDIP  !    ( COLD IP )
'  FORTH  6  +  CVOCLINK  !  ( COLD VOC-LINK )
