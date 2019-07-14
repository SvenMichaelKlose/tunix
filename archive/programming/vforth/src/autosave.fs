decimal

: autostart   ( -- )
    sp!  forth-83 ( replaced with user word )  cold  ;

: autosave   ( cfa -- )
    ['] autostart  swap  over  >body  2+  !  ( save caller's word )
    name  pad  over  c@  1+  cmove>  pad  count  saved  ( save dictionary, run autostart on load )
    ?ioerr  ;
