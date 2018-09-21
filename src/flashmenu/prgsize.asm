;
; Ullrich von Bassewitz, 2010-11-13
; Adopted by Sven Michael Klose <pixel@hugbox.org>
;
; This module supplies the program size that is expected by the g operating
; system in the first two bytes of an excutable disk file.
;


        ; The following symbol is used by linker config to force the module
        ; to get included into the output file
        .export     __PRGSIZE__: absolute = 1
        .import     __PRGEND__

.segment        "PRGSIZE"

        .addr   __PRGEND__ - * - 4
        .addr   $8000 - __PRGEND__  ; BSS size.
