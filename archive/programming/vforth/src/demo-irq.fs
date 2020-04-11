include assembler.fs
include interrupt.fs

( this word will be called on each timer expiry )
: isr
    36879  c@  dup 248  and  swap  1+  7  and  or  36879  c!  ;

' isr  interrupt  irqw!

: test
   interrupt  setup
    begin
	42 emit
    ?terminal  until
    interrupt  restore  ;

forth
