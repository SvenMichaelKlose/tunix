( Simple examples )

( Leave either 205 or 206 on the stack, use VIC raster register )
( as a pseudo random number. )
: wall   ( -- char )
    205  36868  c@  3  rshift  1  and  +  ;

( Draw a random maze until STOP is pressed. )
: maze   ( -- )
    cr  begin
    wall  emit  ?terminal  until  ;


( File I/O examples )

: readfile   ( lfh -- )
    $chkin  ?ioerr
    begin
	pad  255  $readline  dup  ?ioerr
	pad  count  type  cr
    ?terminal  or  until
    $rstin  ;

( Display the contents of a disk file on the screen. )
: cat
    name  count
    device#  @  nextsa  open-file  ?ioerr
    dup  readfile  close-file  ?ioerr  ;
