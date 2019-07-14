vocabulary debugger immediate
debugger definitions

hex

20 create rstack
dup  2+  here  +  dup  ,  ,  allot

variable ip
0 ip !

: rp!
    rstack  2+  @  rstack  !  ;

: >r   ( n -- )
    rstack  @  rstack  2+  =  20 ?error
    rstack  @  !  rstack  @  2-  rstack  !  ;

: r>   ( -- n )
    rstack  2+  @  rstack  @  =  21 ?error
    rstack  @  2+  rstack  !  rstack  @  @  ;

: r@   ( -- n )
    rstack  2+  @  rstack  @  =  21 ?error
    rstack  @  2+  @  ;

: rpick   ( p -- n )
    1+  2*  rstack  @  +  @  ;

: rdepth   ( -- n )
    rstack  2+  @  rstack  @  -  2/  ;

: r.
    23  emit  rdepth  .  3e  emit
    rstack  2+  @  rstack  @  ?do  i  2+  @  .  2  +loop  ;

: s.
    23  emit  depth  .  3e  emit
    60  sp@  2+  ?do  i  @  .  2  +loop  ;

: lit
    ip  @  dup  2+  ip  !  @  ;

: clit
    ip  @  dup  1+  ip  !  c@  ;

: branch
    ip  @  @  ip  !  ;

: ?branch
    if  ip  @  2+  ip  !  else  branch  then  ;

: exit
    rdepth  if
	r>  else
	0  then  ip  !  ;

: (")
    ip  @  dup  c@  1+  over  +  ip  !  ;

: (.")
    (")  count  type  ;

: (abort")
    (")  swap  if  count  type  abort  then  drop  ;

: (do)   ( limit start  -- )
    ip  @  >r  over  >r  swap  -  >r  ip  @  2+  ip  !  ;

: (?do)   ( limit start  -- )
    2dup  =  if
	2drop  ip  @  2+  @  ip  !  else
	(do)  then  ;

: leave
    r>  drop  r>  drop  r>  @  ip  !  ;

: (+loop)   ( inc  -- )
    r>  1  over  +-  swap  rot  +  1  over  +-  swap  >r  =  if
	2  rpick  2+  ip  !  else
	leave  then  ;

: (loop)
    1  (+loop)  ;

: i
    r@  1  rpick  +  ;

: j
    3  rpick  4  rpick  +  ;

( emulation table )

create emulate
  forth  ' lit      ,  debugger  ' lit      ,
  forth  ' clit     ,  debugger  ' clit     ,
  forth  ' branch   ,  debugger  ' branch   ,
  forth  ' ?branch  ,  debugger  ' ?branch  ,
  forth  ' leave    ,  debugger  ' leave    ,
  forth  ' (loop)   ,  debugger  ' (loop)   ,
  forth  ' (+loop)  ,  debugger  ' (+loop)  ,
  forth  ' (do)     ,  debugger  ' (do)     ,
  forth  ' (?do)    ,  debugger  ' (?do)    ,
  forth  ' i        ,  debugger  ' i        ,
  forth  ' j        ,  debugger  ' j        ,
  forth  ' rp!      ,  debugger  ' rp!      ,
  forth  ' exit     ,  debugger  ' exit     ,
  forth  ' >r       ,  debugger  ' >r       ,
  forth  ' r>       ,  debugger  ' r>       ,
  forth  ' r@       ,  debugger  ' r@       ,
  forth  ' (.")     ,  debugger  ' (.")     ,
  forth  ' (abort") ,  debugger  ' (abort") ,
  forth  ' (")      ,  debugger  ' (")      ,
-1 ,

variable dfltn
0 dfltn !
variable dflto
3 dflto !

: exec   ( cfa -- )
    emulate  begin
	2dup  @  swap  u<  while
	    4  +
    repeat
    2dup  @  =  if   ( entry found )
	nip  2+  @  else
	drop  then
    dfltn  @  99  c!  dflto  @  9a  c!  execute
    99  c@  dfltn  !  9a  c@  dflto  !  0  99  c!  3  9a  c!  ;

: name.   ( cfa -- )
    12  emit  dup  [']  lit  =  if
	drop  ip  @  @  .
    else  >name  id.  0  0d4  c!  then
    92  emit  ;

: ?done
    ip  @  0=  if  ." trace done"  quit  then  ;

: n
    ?done  ip  @  dup  2+  ip  !  @  dup  name.  exec  ;

: s
    ?done  ip  @  @  @  ['] exec  @  =  if
	ip  @  dup  2+  >r  @  dup  name.  >body  ip  !  else
	n  then  ;

: w
    ?done  ip  @  @  name.  ;

forth definitions
decimal

: dump   ( addr count -- )
   ?dup  if
   base  @  >r  hex  cr  swap  >r  0  begin
      dup  8  +  2  pick  min  r@  over  >r  swap  rot
      2  pick  over  +  u.  do
         i  over  +  c@  s->d  <#  #  #  #>  type
      loop  cr  drop  r>  2dup  =  until
   r>  drop  2drop  r>  base  !  else
   drop  then  ;

: trace
    '  >body  debugger  ip  !  [compile]  debugger  ;

forth
