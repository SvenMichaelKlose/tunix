decimal

16 +origin constant coldip

: autostart   ( -- )
    sp!  forth-83 ( replaced with user word )  basic  ;

: autosave   ( cfa -- )
    ['] autostart  >body  tuck  2+  !  ( save caller's word )
    coldip  @  swap  coldip  !  dsave  coldip  !  ;
