include? dos dos.fs
include? graphics graphics.fs

( Map of track to number of sectors per track )
create sectors
31  c,  17  c,
25  c,  18  c,
18  c,  19  c,
1   c,  21  c,

( Area to hold Block Availability Map )
create bam
256  allot

( Area to hold buffer read from disk )
create buffer
256  allot

( Number of tracks )
36 constant #tracks

( Return number of sectors for a track )
: #sectors   ( track  --  #sectors )
    sectors  begin
	2dup  c@  <  while
	    2+  repeat
    1+  c@  nip  ;

variable srclfn
variable dstlfn

( Read Block Availability Map )
: readbam   ( lfn  --  )   dos
    dup  18  0  req-read  bam  swap  read-blk  ;

( Test if a block is in use )
: used?   ( track sector  --  fl )
    swap  2  lshift  1+  bam  +  over  3  rshift  +  c@
    swap  1  swap  7  and  lshift  and  0=  ;

graphics  22  23  0  mode text-mode

( Map of half-character pixels )
create tpix
96  c,  225  c,  97  c,  224  c,

( Convert bits to pixel )
: bit2pix   ( bits  --  pix )
    tpix  +  c@  ;

( Convert pixel to bits )
: pix2bit   ( pix  --  bits )
    0  swap  4  0  do
	dup  tpix  i  +  c@  =  if
	    drop  i  swap  leave
	then  loop
    drop  ;

( Plot a half-character pixel )
: tplot   ( x y  --  )   graphics
    over  1  rshift  swap  char-at  swap  not  1  and  1+  over  c@
    pix2bit  or  bit2pix  swap  c!  ;

( Unplot a half-character pixel )
: tunplot  ( x y  --  )   graphics
    over  1  rshift  swap  char-at  swap  1  and  1+  over  c@
    pix2bit  and  bit2pix  swap  c!  ;

( Show graphical representation of BAM )
: plotbam
    #tracks  1  do   ( for each track )
	i  #sectors  0  do   ( for each sector )
	    j  i  used?  if
		j  i  tplot
	    then
	loop
    loop  ;

( Copy one sector)
: copysector  ( track sector  --  )   dos
    2dup  tunplot  ( clear pixel )
    2dup  srclfn  @  rot  rot  req-read
    buffer  srclfn  @  read-blk
    dstlfn  @  0  req-pos
    buffer  dstlfn  @  write-blk
    dstlfn  @  rot  rot  req-write  ;

( Copy a whole track )
: copytrack   ( track  --  )
    dup  #sectors  0  do  ( for each sector )
	dup  i  2dup  used?  if  copysector
	else  2drop  then
    loop  drop  ;

( Prompt for a string )
: instr   (  --  addr )
    pad  22  expect  bl  span  @  pad  +  c!
    pad  1-  span  @  over  c!  ;

( Prompt for a number )
: val   (  --  n )
    instr  number  drop  ;

( Prompt for source device and open a command channel )
: opensrc   dos
    cr  ." enter source device"  cr  val  dup  device#  !
    cmd-lfn  drop  ( open command channel first )
    open-chn  srclfn  !  ;

( Prompt for destination device and open a command channel )
: opendst   dos
    cr  ." enter dest device"  cr  val  dup  device#  !
    cmd-lfn  drop  ( open command channel first )
    open-chn  dstlfn  !  ;

( Close source file )
: closesrc
    srclfn  @  close-file  ;

( Close destination channel )
: closedst   dos
    dstlfn  @  close-file
    [char] i  pad  c!  drive#  @  [char] 0  +  pad  1+  c!
    pad  2  dos-cmd  type  ;

( Copy the contents of a device to another )
: copy
    opensrc  opendst  cr  srclfn  @  readbam
    graphics  text-mode  mode-set  clear-screen  plotbam
    #tracks  1  do  ( for each track )
	i  copytrack
    loop  closesrc  closedst  dos  #8  ;
