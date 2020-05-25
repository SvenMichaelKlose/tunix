vocabulary dos immediate
dos definitions
hex

variable drive#
0  drive#  !

: #8    8  device#  !  ;
: #9    9  device#  !  ;

( Return device of an LFN )
: finddev   ( lfn  --  da )
    -1  swap
    98  c@  0  ?do   ( loop over all open files )
	dup  259  i  +  c@  =  if   ( if LFN matches )
	    drop  263  i  +  c@  swap  leave
	then  loop
    drop  ;

( Return secondary address of an LFN )
: findsa   ( lfn  --  sa )
    -1  swap
    98  c@  0  ?do   ( loop over all open files )
	dup  259  i  +  c@  =  if   ( if LFN matches )
	    drop  26d  i  +  c@  1f  and  swap  leave
	then  loop
    drop  ;

( Return the LFN associated with a given device & secondary address )
: findlfn   ( da sa  --  lfn )
    0  rot  rot
    98  c@  0  ?do   ( loop over all open files )
	over  263  i  +  c@  =  if   ( if device matches )
	    dup  26d  i  +  c@  1f  and  =  if   ( and secondary )
		rot  drop  259  i  +  c@  rot  rot  leave
	    then  then  loop
    2drop  ;

( Return the LFN of the command channel for a device, opening a new file if )
( necessary )
: cmd-lfn   ( da  --  lfn )
    dup  0f  findlfn  ?dup  if
	nip
    else   ( not yet open )
	0  0  $setnam  0f  swap  nextlfn  >r
	r@  $setlfs  r>  $open  if
	    drop  0  then
    then  ;

( Read one directory entry into PAD, first byte contains the length of the )
( variable part )
: read-dir-rec   (  --  )
    0  pad  c!  pad  1+  4  $read  0=  swap  4  =  and  if
	pad  4  +  begin
	    1+  dup  1  $read  swap  drop  dup  0=  if
		pad  c@  1+  pad  c!  over  c@  0=  or  then
	until  drop
    then  ;

( Read from LFN and print each directory entry )
: print-dir   ( lfh  --  )
    $chkin  ?ioerr  pad  2  $read  2drop   ( skip start addr )
    begin
	read-dir-rec  pad  c@  dup  if
	    cr  pad  3  +  @  .  pad  5  +  swap  type
	    ?terminal  not  then  0=
    until  cr  $rstin  ;

( print the directory entries matching string )
: dir   ( addr count  --  )
    0  open-file  ?ioerr  dup  print-dir  close-file  ?ioerr  ;

: $"
    22  word  >r  r@  c@  1+  [char] $  r@  c!  r>  swap  dir  ;

( Print the directory of the default device )
: $   (  --  )
    [char] $  pad  c!  pad  1  dir  ;

( Execute a DOS command on the default device, return the result )
: dos-cmd   ( addr count  --  addr count )
    device#  @  cmd-lfn  >r  ?dup  if
	r@  write-line  drop  else
	drop  then
    pad  20  r>  read-line  drop  pad  count  ;

( Print the error status of the default device )
: @.
    0  0  dos-cmd  type  ;

( Execute a DOS command and print the error status )
: @"
    22  word  count  dos-cmd  type  ;

( Open channel on current device)
: open-chn   (  --  lfn )
    s" #"  device#  @  nextsa  open-file  ?ioerr  ;

( Request track/sector read )
: req-read   ( lfn t s  --  )
    2  pick  finddev  cmd-lfn  $chkout  ?ioerr  >r  ." u1:"  rot
    findsa  .  drive#  @  .  swap  .  .  cr  r>  $rstout  ;

( Read block from channel )
: read-blk   ( addr lfn  --  )
    100  swap  read-file  ?ioerr  100  =  not  05a  ?error  ;

( Request track/sector write )
: req-write  ( lfn t s  --  )
    2  pick  finddev  cmd-lfn  $chkout  ?ioerr  >r  ." u2:"  rot
    findsa  .  drive#  @  .  swap  .  .  cr  r>  $rstout  ;

( Write block to channel )
: write-blk   ( addr lfn  --  )
    100  swap  write-file  ?ioerr  ;

( Set block position )
: req-pos  ( lfn pos  --  )
    over  finddev  cmd-lfn  $chkout  ?ioerr  >r  ." b-p "  swap
    findsa  .  .  cr  r>  $rstout  ;

( Create new relative file )
: rel-create  ( addr count reclen  --  lfn )
    >r  tuck  100  swap  cmove  100  over  +
    [char] ,  over  c!  1+  [char] l  over  c!  1+
    [char] ,  over  c!  1+  r>  swap  c!  4  +
    100  swap  device#  @  nextsa  open-file  ?ioerr  ;

( Set relative file position )
: rel-pos  ( lfn pos  --  )
    over  finddev  cmd-lfn  $chkout  ?ioerr  >r  [char] p  emit  swap
    findsa  emit  100  /mod  swap  emit  emit  1  emit  cr  r>  $rstout  ;

forth definitions
decimal
