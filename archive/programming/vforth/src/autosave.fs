decimal

' lit 0< 31 ?error  ( this process does not work with the ROM image )

16 +origin constant coldip

: autostart   ( -- )
    sp!  forth-83 ( replaced with user word )  basic  ;

: autosave   ( cfa -- )
    ['] autostart  >body  tuck  2+  !  ( save caller's word )
    coldip  @  swap  coldip  !  dsave  coldip  !  ;
