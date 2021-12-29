;***********************************************************************************;
;***********************************************************************************;
;
; The almost completely commented VIC 20 ROM disassembly. V1.01 Lee Davison 2005-2012.
; With enhancements by Simon Rowe <srowe@mose.org.uk>.

; This is a bit correct assembly listing for the VIC 20 BASIC and KERNAL ROMs as one 16K
; ROM. You should be able to assemble the VIC ROMs from this with most 6502 assemblers,
; as no macros or 'special' features were used. This has been tested using Michal
; Kowalski's 6502 Simulator assemble function. See http://exifpro.com/utils.html for
; this program.

; Many references were used to complete this disassembly including, but not limited to,
; "Mapping the VIC 20", "Mapping the C64", "VIC 20 Programmers Reference", "VIC 20 User
; Guide", "The Complete Commodore Inner Space Anthology", "VIC Revealed" and various
; text files, pictures and other documents.


;***********************************************************************************;
;***********************************************************************************;
;
; BASIC zero page

; These locations contain the JMP instruction target address of the USR command. They
; are initialised so that if you try to execute a USR call without changing them you
; will receive an ILLEGAL QUANTITY error message.

USRPPOK = $00           ; USR() JMP instruction
ADDPRC  = $01           ; USR() vector

; This vector points to the address of the BASIC routine which converts a floating point
; number to an integer, however BASIC does not use this vector. It may be of assistance
; to the programmer who wishes to use data that is stored in floating point format. The
; parameter passed by the USR command is available only in that format for example.

ADRAY1  = $03           ; float to fixed vector

; This vector points to the address of the BASIC routine which converts an integer to a
; floating point number, however BASIC does not use this vector. It may be used by the
; programmer who needs to make such a conversion for a machine language program that
; interacts with BASIC. To return an integer value with the USR command for example.

ADRAY2  = $05           ; fixed to float vector

; The cursor column position prior to the TAB or SPC is moved here from PNTR, and is used
; to calculate where the cursor ends up after one of these functions is invoked.

; Note that the value contained here shows the position of the cursor on a logical line.
; Since one logical line can be up to four physical lines long, the value stored here
; can range from 0 to 87.

CHARAC  = $07           ; search character
ENDCHR  = $08           ; scan quotes flag
TRMPOS  = $09           ; TAB column save

; The routine that converts the text in the input buffer into lines of executable program
; tokens, and the routines that link these program lines together, use this location as an
; index into the input buffer area. After the job of converting text to tokens is done,
; the value in this location is equal to the length of the tokenised line.

; The routines which build an array or locate an element in an array use this location to
; calculate the number of DIMensions called for and the amount of storage required for a
; newly created array, or the number of subscripts when referencing an array element.

VERCHK  = $0A           ; load/verify flag, 0 = load, 1 = verify
COUNT   = $0B           ; temporary byte, line crunch/array access/logic operators

; This is used as a flag by the routines that build an array or reference an existing
; array. It is used to determine whether a variable is in an array, whether the array
; has already been DIMensioned, and whether a new array should assume the default size.

DIMFLG  = $0C           ; DIM flag

; This flag is used to indicate whether data being operated upon is string or numeric. A
; value of $FF in this location indicates string data while a $00 indicates numeric data.

VALTYP  = $0D           ; data type flag, $FF = string, $00 = numeric

; If the above flag indicates numeric then a $80 in this location identifies the number
; as an integer, and a $00 indicates a floating point number.

INTFLG  = $0E           ; data type flag, $80 = integer, $00 = floating point

; The garbage collection routine uses this location as a flag to indicate that garbage
; collection has already been tried before adding a new string. If there is still not
; enough memory, an OUT OF MEMORY error message will result.

; LIST uses this byte as a flag to let it know when it has come to a character string in
; quotes. It will then print the string, rather than search it for BASIC keyword tokens.

; This location is also used during the process of converting a line of text in the BASIC
; input buffer into a linked program line of BASIC keyword tokens to flag a DATA line is
; being processed.

GARBFL  = $0F           ; garbage collected/open quote/DATA flag

; If an opening parenthesis is found, this flag is set to indicate that the variable in
; question is either an array variable or a user-defined function.

SUBFLG  = $10           ; subscript/FNx flag

; This location is used to determine whether the sign of the value returned by the
; functions SIN, COS, ATN or TAN is positive or negative.

; Also the comparison routines use this location to indicate the outcome of the compare.
; For A <=> B the value here will be $01 if A > B, $02 if A = B, and $04 if A < B. If
; more than one comparison operator was used to compare the two variables then the value
; here will be a combination of the above values.

INPFLG  = $11           ; input mode flag, $00 = INPUT, $40 = GET, $98 = READ
TANSGN  = $12           ; ATN sign/comparison evaluation flag

; When the default input or output device is used the value here will be a zero, and the
; format of prompting and output will be the standard screen output format. The location
; $B8 is used to decide what device actually to put input from or output to.

CHANNL  = $13           ; current I/O channel

; Used whenever a 16 bit integer is used e.g. the target line number for GOTO, LIST, ON,
; and GOSUB also the number of a BASIC line that is to be added or replaced. Additionally
; PEEK, POKE, WAIT, and SYS use this location as a pointer to the address which is the
; subject of the command.

LINNUM  = $14           ; temporary integer low byte
;     $15           ; temporary integer high byte

; This location points to the next available slot in the temporary string descriptor
; stack located at TEMPST.

TEMPPT  = $16           ; descriptor stack pointer, next free

; This contains information about temporary strings which have not yet been assigned to
; a string variable.

LASTPT  = $17           ; current descriptor stack item pointer low byte
;     $18           ; current descriptor stack item pointer high byte
TEMPST  = $19           ; to $21, descriptor stack

; These locations are used by BASIC multiplication and division routines. They are also
; used by the routines which compute the size of the area required to store an array
; which is being created.

INDEX   = $22           ; misc temp byte
;     $23           ; misc temp byte
;     $24           ; misc temp byte
;     $25           ; misc temp byte

RESHO   = $26           ; temp mantissa 1
;     $27           ; temp mantissa 2
;     $28           ; temp mantissa 3
;     $29           ; temp mantissa 4

; Word pointer to where the BASIC program text is stored.

TXTTAB  = $2B           ; start of memory low byte
;     $2C           ; start of memory high byte

; Word pointer to the start of the BASIC variable storage area.

VARTAB  = $2D           ; start of variables low byte
;     $2E           ; start of variables high byte

; Word pointer to the start of the BASIC array storage area.

ARYTAB  = $2F           ; end of variables low byte
;     $30           ; end of variables high byte

; Word pointer to end of the start of free RAM.

STREND  = $31           ; end of arrays low byte
;     $32           ; end of arrays high byte

; Word pointer to the bottom of the string text storage area.

FRETOP  = $33           ; bottom of string space low byte
;     $34           ; bottom of string space high byte

; Used as a temporary pointer to the most current string added by the routines which
; build strings or move them in memory.

FRESPC  = $35           ; string utility ptr low byte
;     $36           ; string utility ptr high byte

; Word pointer to the highest address used by BASIC +1.

MEMSIZ  = $37           ; end of memory low byte
;     $38           ; end of memory high byte

; These locations contain the line number of the BASIC statement which is currently being
; executed. A value of $FF in location $3A means that BASIC is in immediate mode.

CURLIN  = $39           ; current line number low byte
;     $3A           ; current line number high byte

; When program execution ends or stops the last line number executed is stored here.

OLDLIN  = $3B           ; break line number low byte
;     $3C           ; break line number high byte

; These locations contain the address of the start of the text of the BASIC statement
; that is being executed. The value of the pointer to the address of the BASIC text
; character currently being scanned is stored here each time a new BASIC statement begins
; execution.

OLDTXT  = $3D           ; continue pointer low byte
;     $3E           ; continue pointer high byte

; These locations hold the line number of the current DATA statement being READ. If an
; error concerning the DATA occurs this number will be moved to CURLIN so that the error
; message will show the line that contains the DATA statement rather than in the line that
; contains the READ statement.

DATLIN  = $3F           ; current DATA line number low byte
;     $40           ; current DATA line number high byte

; These locations point to the address where the next DATA will be READ from. RESTORE
; sets this pointer back to the address indicated by the start of BASIC pointer.

DATPTR  = $41           ; DATA pointer low byte
;     $42           ; DATA pointer high byte

; READ, INPUT and GET all use this as a pointer to the address of the source of incoming
; data, such as DATA statements, or the text input buffer.

INPPTR  = $43           ; READ pointer low byte
;     $44           ; READ pointer high byte

VARNAM  = $45           ; current variable name first byte
;     $46           ; current variable name second byte

; These locations point to the value of the current BASIC variable. Specifically they
; point to the byte just after the two-character variable name.

VARPNT  = $47           ; current variable address low byte
;     $48           ; current variable address high byte

; The address of the BASIC variable which is the subject of a FOR/NEXT loop is first
; stored here before being pushed onto the stack.

FORPNT  = $49           ; FOR/NEXT variable pointer low byte
;     $4A           ; FOR/NEXT variable pointer high byte

; The expression evaluation routine creates this to let it know whether the current
; comparison operation is a < $01, = $02 or > $04 comparison or combination.

OPPTR   = $4B           ; BASIC execute pointer temporary low byte/precedence flag
;     $4C           ; BASIC execute pointer temporary high byte
OPMASK  = $4D           ; comparison evaluation flag

; These locations are used as a pointer to the function that is created during function
; definition. During function execution it points to where the evaluation results should
; be saved.

DEFPNT  = $4E           ; FAC temp store/function/variable/garbage pointer low byte
;     $4F           ; FAC temp store/function/variable/garbage pointer high byte

; Temporary pointer to the current string descriptor.

DSCPTN  = $50           ; FAC temp store/descriptor pointer low byte
;     $51           ; FAC temp store/descriptor pointer high byte

FOUR6   = $53           ; garbage collection step size

; The first byte is the 6502 JMP instruction $4C, followed by the address of the required
; function taken from the table at FUNDSP.

JMPER   = $54           ; JMP opcode for functions
;     $55           ; functions jump vector

TEMPF3  = $57           ; FAC temp store
GENPTR  = $58           ; FAC temp store
;     $59           ; FAC temp store
GEN2PTR = $5A           ; FAC temp store
;     $5B           ; block end high byte
LAB_5C  = $5C           ; FAC temp store
LAB_5D  = $5D           ; FAC temp store
EXPCNT  = $5E           ; exponent count byte
;     $5F           ; FAC temp store
TMPPTR  = $5F
;     $60           ; block start high byte
FAC1    = $61           ; FAC1 exponent
;     $62           ; FAC1 mantissa 1
;     $63           ; FAC1 mantissa 2
;     $64           ; FAC1 mantissa 3
;     $65           ; FAC1 mantissa 4
;     $66           ; FAC1 sign
SGNFLG  = $67           ; constant count/-ve flag
BITS    = $68           ; FAC1 overflow
FAC2    = $69           ; FAC2 exponent
;     $6A           ; FAC2 mantissa 1
;     $6B           ; FAC2 mantissa 2
;     $6C           ; FAC2 mantissa 3
;     $6D           ; FAC2 mantissa 4
;     $6E           ; FAC2 sign
ARISGN  = $6F           ; FAC sign comparison
FACOV   = $70           ; FAC1 rounding
FBUFPT  = $71           ; temp BASIC execute/array pointer low byte/index
;     $72           ; temp BASIC execute/array pointer high byte

CHRGET  = $73           ; increment and scan memory, BASIC byte get
CHRGOT  = $79           ; scan memory, BASIC byte get
;     $7A           ; BASIC execute pointer low byte
;     $7B           ; BASIC execute pointer high byte
CHRSPC  = $80           ; numeric test entry

RNDX    = $8B           ; RND() seed, five bytes


; KERNAL zero page

STATUS  = $90           ; serial status byte
                    ;   function
                    ; bit   cassette        serial bus
                    ; ---   --------        ----------
                    ;  7    end of tape     device not present
                    ;  6    end of file     EOI
                    ;  5    checksum error
                    ;  4    read error
                    ;  3    long block
                    ;  2    short block
                    ;  1                time out read
                    ;  0                time out write
STKEY   = $91           ; keyboard row, bx = 0 = key down
                    ; bit   key
                    ; ---   ------
                    ;  7    [DOWN]
                    ;  6    /
                    ;  5    ,
                    ;  4    N
                    ;  3    V
                    ;  2    X
                    ;  1    [L SHIFT]
                    ;  0    [STOP]
SVXT    = $92           ; timing constant for tape read
VERCK   = $93           ; load/verify flag, load = $00, verify = $01
C3PO    = $94           ; serial output: deferred character flag
                    ; $00 = no character waiting, $xx = character waiting
BSOUR   = $95           ; serial output: deferred character
                    ; $FF = no character waiting, $xx = waiting character
SYNO    = $96           ; cassette block synchronisation number
XSAV    = $97           ; register save

; The number of currently open I/O files is stored here. The maximum number that can be
; open at one time is ten. The number stored here is used as the index to the end of the
; tables that hold the file numbers, device numbers, and secondary addresses.

LDTND   = $98           ; open file count

; The default value of this location is 0.

DFLTN   = $99           ; input device number

; The default value of this location is 3.

DFLTO   = $9A           ; output device number
                    ;
                    ; number    device
                    ; ------    ------
                    ;  0        keyboard
                    ;  1        cassette
                    ;  2        RS-232
                    ;  3        screen
                    ;  4-31     serial bus

PRTY    = $9B           ; tape character parity
DPSW    = $9C           ; byte received flag
MSGFLG  = $9D           ; message mode flag,
                    ; $C0 = both control and KERNAL messages,
                    ; $80 = control messages only,
                    ; $40 = KERNAL messages only,
                    ; $00 = neither control nor KERNAL messages
PTR1    = $9E           ; tape pass 1 error log/character buffer
PTR2    = $9F           ; tape pass 2 error log corrected

; These three locations form a counter which is updated 60 times a second, and serves as
; a software clock which counts the number of jiffies that have elapsed since the computer
; was turned on. After 24 hours and one jiffy these locations are set back to $000000.

TIME    = $A0           ; jiffy clock high byte
;     $A1           ; jiffy clock mid byte
;     $A2           ; jiffy clock low byte

PCNTR   = $A3           ; EOI flag byte/tape bit count

; b0 of this location reflects the current phase of the tape output cycle.

FIRT    = $A4           ; tape bit cycle phase
CNTDN   = $A5           ; cassette synchronisation byte count/serial bus bit count
BUFPNT  = $A6           ; tape buffer index
INBIT   = $A7           ; receiver input bit temp storage
BITCI   = $A8           ; receiver bit count in
RINONE  = $A9           ; receiver start bit check flag, $90 = no start bit
                    ; received, $00 = start bit received
RIDATA  = $AA           ; receiver byte buffer/assembly location
RIPRTY  = $AB           ; receiver parity bit storage
SAL = $AC           ; tape buffer start pointer low byte
                    ; scroll screen ?? byte
;     $AD           ; tape buffer start pointer high byte
                    ; scroll screen ?? byte
EAL = $AE           ; tape buffer end pointer low byte
                    ; scroll screen ?? byte
;     $AF           ; tape buffer end pointer high byte
                    ; scroll screen ?? byte
CMP0    = $B0           ; tape timing constant min byte
;     $B1           ; tape timing constant max byte

; These two locations point to the address of the cassette buffer. This pointer must
; be greater than or equal to $0200 or an ILLEGAL DEVICE NUMBER error will be sent
; when tape I/O is tried. This pointer must also be less that $8000 or the routine
; will terminate early.

TAPE1   = $B2           ; tape buffer start pointer low byte
;     $B3           ; tape buffer start pointer high byte

; RS-232 routines use this to count the number of bits transmitted and for parity and
; stop bit manipulation. Tape load routines use this location to flag when they are
; ready to receive data bytes.

BITTS   = $B4           ; transmitter bit count out

; This location is used by the RS-232 routines to hold the next bit to be sent and by the
; tape routines to indicate what part of a block the read routine is currently reading.

NXTBIT  = $B5           ; transmitter next bit to be sent

; RS-232 routines use this area to disassemble each byte to be sent from the transmission
; buffer pointed to by ROBUF.

RODATA  = $B6           ; transmitter byte buffer/disassembly location

; Disk filenames may be up to 16 characters in length while tape filenames may be up to
; 187 characters in length.

; If a tape name is longer than 16 characters the excess will be truncated by the
; SEARCHING and FOUND messages, but will still be present on the tape.

; A disk file is always referred to by a name. This location will always be greater than
; zero if the current file is a disk file.

; An RS-232 OPEN command may specify a filename of up to four characters. These characters
; are copied to M51CTR to M51CTR+3 and determine baud rate, word length, and parity, or
; they would do if the feature was fully implemented.

FNLEN   = $B7           ; file name length

LA  = $B8           ; logical file
SA  = $B9           ; secondary address
FA  = $BA           ; current device number
                    ; number    device
                    ; ------    ------
                    ;  0        keyboard
                    ;  1        cassette
                    ;  2        RS-232
                    ;  3        screen
                    ;  4-31     serial bus
FNADR   = $BB           ; file name pointer low byte
;     $BC           ; file name pointer high byte
ROPRTY  = $BD           ; tape write byte/RS-232 parity byte

; Used by the tape routines to count the number of copies of a data block remaining to
; be read or written.

FSBLK   = $BE           ; tape copies count
MYCH    = $BF           ; parity count ??
CAS1    = $C0           ; tape motor interlock
STAL    = $C1           ; I/O start addresses low byte
;     $C2           ; I/O start addresses high byte
MEMUSS  = $C3           ; KERNAL setup pointer low byte
;     $C4           ; KERNAL setup pointer high byte
LSTX    = $C5           ; current key pressed
                    ;
                    ;  # key     # key       # key       # key
                    ; -- ---    -- ---      -- ---      -- ---
                    ; 00 1      10 none     20 [SPACE]  30 Q
                    ; 01 3      11 A        21 Z        31 E
                    ; 02 5      12 D        22 C        32 T
                    ; 03 7      13 G        23 B        33 U
                    ; 04 9      14 J        24 M        34 O
                    ; 05 +      15 L        25 .        35 @
                    ; 06 £         16 ;        26 none     36 ^
                    ; 07 [DEL]  17 [CSR R]  27 [F1]     37 [F5]
                    ; 08 [<-]   18 [STOP]   28 none     38 2
                    ; 09 W      19 none     29 S        39 4
                    ; 0A R      1A X        2A F        3A 6
                    ; 0B Y      1B V        2B H        3B 8
                    ; 0C I      1C N        2C K        3C 0
                    ; 0D P      1D ,        2D :        3D -
                    ; 0E *      1E /        2E =        3E [HOME]
                    ; 0F [RET]  1F [CSR D]  2F [F3]     3F [F7]

NDX = $C6           ; keyboard buffer length/index

; When the [CTRL][RVS-ON] characters are printed this flag is set to $12, and the print
; routines will add $80 to the screen code of each character which is printed, so that
; the character will appear on the screen with its colours reversed.

; Note that the contents of this location are cleared not only upon entry of a
; [CTRL][RVS-OFF] character but also at every carriage return.

RVS = $C7           ; reverse flag $12 = reverse, $00 = normal

; This pointer indicates the column number of the last nonblank character on the logical
; line that is to be input. Since a logical line can be up to 88 characters long this
; number can range from 0-87.

INDX    = $C8           ; input [EOL] pointer

; These locations keep track of the logical line that the cursor is on and its column
; position on that logical line.

; Each logical line may contain up to four 22 column physical lines. So there may be as
; many as 23 logical lines, or as few as 6 at any one time. Therefore, the logical line
; number might be anywhere from 1-23. Depending on the length of the logical line, the
; cursor column may be from 1-22, 1-44, 1-66 or 1-88.

; For a more on logical lines, see the description of the screen line link table, $D9.

LXSP    = $C9           ; input cursor row
;     $CA           ; input cursor column

; The keyscan interrupt routine uses this location to indicate which key is currently
; being pressed. The value here is then used as an index into the appropriate keyboard
; table to determine which character to print when a key is struck.

; The correspondence between the key pressed and the number stored here is as follows:

; $00   1   $10 not used    $20 [SPACE]     $30 Q   $40 [NO KEY]
; $01   3   $11 A       $21 Z       $31 E   $xx invalid
; $02   5   $12 D       $22 C       $32 T
; $03   7   $13 G       $23 B       $33 U
; $04   9   $14 J       $24 M       $34 O
; $05   +   $15 L       $25 .       $35 @
; $06   £  $16 ;       $26 not used    $36 ^
; $07   [DEL]   $17 [CRSR R]    $27 [F1]        $37 [F5]
; $08   [<-]    $18 [STOP]      $28 not used    $38 2
; $09   W   $19 not used    $29 S       $39 4
; $0A   R   $1A X       $2A F       $3A 6
; $0B   Y   $1B V       $2B H       $3B 8
; $0C   I   $1C N       $2C K       $3C 0
; $0D   P   $1D ,       $2D :       $3D -
; $0E   *   $1E /       $2E =       $3E [HOME]
; $0F   [RET]   $1F [CRSR D]    $2F [F3]        $3F [F7]

SFDX    = $CB           ; which key

; When this flag is set to a nonzero value, it indicates to the routine that normally
; flashes the cursor not to do so. The cursor blink is turned off when there are
; characters in the keyboard buffer, or when the program is running.

BLNSW   = $CC           ; cursor enable, $00 = flash cursor

; The routine that blinks the cursor uses this location to tell when it's time for a
; blink. The number 20 is put here and decremented every jiffy until it reaches zero.
; Then the cursor state is changed, the number 20 is put back here, and the cycle starts
; all over again.

BLNCT   = $CD           ; cursor timing countdown

; The cursor is formed by printing the inverse of the character that occupies the cursor
; position. If that characters is the letter A, for example, the flashing cursor merely
; alternates between printing an A and a reverse-A. This location keeps track of the
; normal screen code of the character that is located at the cursor position, so that it
; may be restored when the cursor moves on.

CDBLN   = $CE           ; character under cursor

; This location keeps track of whether, during the current cursor blink, the character
; under the cursor was reversed, or was restored to normal. This location will contain
; $00 if the character is reversed, and $01 if the character is not reversed.

BLNON   = $CF           ; cursor blink phase

CRSW    = $D0           ; input from keyboard or screen, $xx = input is available
                    ; from the screen, $00 = input should be obtained from the
                    ; keyboard

; These locations point to the address in screen RAM of the first column of the logical
; line upon which the cursor is currently positioned.

PNT = $D1           ; current screen line pointer low byte
;     $D2           ; current screen line pointer high byte

; This holds the cursor column position within the logical line pointed to by PNT.
; Since a logical line can comprise up to four physical lines, this value may be from
; 0 to 87.

PNTR    = $D3           ; cursor column

; A nonzero value in this location indicates that the editor is in quote mode. Quote
; mode is toggled every time that you type in a quotation mark on a given line, the
; first quote mark turns it on, the second turns it off, the third turns it on, etc.

; If the editor is in this mode when a cursor control character or other nonprinting
; character is entered, a printed equivalent will appear on the screen instead of the
; cursor movement or other control operation taking place. Instead, that action is
; deferred until the string is sent to the string by a PRINT statement, at which time
; the cursor movement or other control operation will take place.

; The exception to this rule is the DELETE key, which will function normally within
; quote mode. The only way to print a character which is equivalent to the DELETE key
; is by entering insert mode. Quote mode may be exited by printing a closing quote or
; by hitting the RETURN or SHIFT-RETURN keys.

QTSW    = $D4           ; cursor quote flag

; The line editor uses this location when the end of a line has been reached to determine
; whether another physical line can be added to the current logical line or if a new
; logical line must be started.

LNMX    = $D5           ; current screen line length

; This location contains the current physical screen line position of the cursor, 0 to 22.

TBLX    = $D6           ; cursor row

; The ASCII value of the last character printed to the screen is held here temporarily.

ASCII   = $D7           ; checksum byte/temporary last character

; When the INST key is pressed, the screen editor shifts the line to the right, allocates
; another physical line to the logical line if necessary (and possible), updates the
; screen line length in LNMX, and adjusts the screen line link table at LDTB1. This location
; is used to keep track of the number of spaces that has been opened up in this way.

; Until the spaces that have been opened up are filled, the editor acts as if in quote
; mode. See location QTSW, the quote mode flag. This means that cursor control characters
; that are normally nonprinting will leave a printed equivalent on the screen when
; entered, instead of having their normal effect on cursor movement, etc. The only
; difference between insert and quote mode is that the DELETE key will leave a printed
; equivalent in insert mode, while the INSERT key will insert spaces as normal.

INSRT   = $D8           ; insert count

; This table contains 23 entries, one for each row of the screen display. Each entry has
; two functions. Bits 0-3 indicate on which of the four pages of screen memory the first
; byte of memory for that row is located. This is used in calculating the pointer to the
; starting address of a screen line at PNT.
;
; The high byte is calculated by adding the value of the starting page of screen memory
; held in HIBASE to the displacement page held here.
;
; The other function of this table is to establish the makeup of logical lines on the
; screen. While each screen line is only 22 characters long, BASIC allows the entry of
; program lines that contain up to 88 characters. Therefore, some method must be used
; to determine which physical lines are linked into a longer logical line, so that this
; longer logical line may be edited as a unit.
;
; The high bit of each byte here is used as a flag by the screen editor. That bit is set
; when a line is the first or only physical line in a logical line. The high bit is reset
; to 0 only when a line is an extension to this logical line.

LDTB1   = $D9           ; to LDTB1 + $18 inclusive, screen line link table

; This pointer is synchronised with the pointer to the address of the first byte of
; screen RAM for the current line kept in PNT. It holds the address of the first byte
; of colour RAM for the corresponding screen line.

LLNKSV  = $F2           ; screen row marker
USER    = $F3           ; colour RAM pointer low byte
;     $F4           ; colour RAM pointer high byte

; This pointer points to the address of the keyboard matrix lookup table currently being
; used. Although there are only 64 keys on the keyboard matrix, each key can be used to
; print up to four different characters, depending on whether it is struck by itself or
; in combination with the SHIFT, CTRL, or C= keys.

; These tables hold the ASCII value of each of the 64 keys for one of these possible
; combinations of keypresses. When it comes time to print the character, the table that
; is used determines which character is printed.

; The addresses of the tables are:

;   NORMKEYS            ; unshifted
;   SHFTKEYS            ; shifted
;   LOGOKEYS            ; commodore
;   CTRLKEYS            ; control

KEYTAB  = $F5           ; keyboard pointer low byte
;     $F6           ; keyboard pointer high byte

; When device the RS-232 channel is opened two buffers of 256 bytes each are created at
; the top of memory. These locations point to the address of the one which is used to
; store characters as they are received.

RIBUF   = $F7           ; RS-232 Rx pointer low byte
;     $F8           ; RS-232 Rx pointer high byte

; These locations point to the address of the 256 byte output buffer that is used for
; transmitting data to RS-232 devices.

ROBUF   = $F9           ; RS-232 Tx pointer low byte
;     $FA           ; RS-232 Tx pointer high byte

; $FB to $FE - unused

BASZPT  = $FF           ; FAC1 to string output base

STACK   = $0100         ; bottom of the stack page

CHNLNK  = $01FC         ; chain link pointer high byte
;   = $01FD         ; chain link pointer low byte

PREVLN  = $01FE         ; line number low byte before crunched line
;     $01FF         ; line number high byte before crunched line

BUF = $0200         ; input buffer, for some routines the byte before the input
                ; buffer needs to be set to a specific value for the routine
                ; to work correctly

LAT = $0259         ; .. to $0262 logical file table
FAT = $0263         ; .. to $026C device number table
SAT = $026D         ; .. to $0276 secondary address table
KEYD    = $0277         ; .. to $0280 keyboard buffer
MEMSTR  = $0281         ; OS start of memory low byte
;     $0282         ; OS start of memory high byte
MEMHIGH = $0283         ; OS top of memory low byte
;     $0284         ; OS top of memory high byte
TIMOUT  = $0285         ; IEEE-488 bus timeout flag ( unused )
COLOR   = $0286         ; current colour code
GDCOL   = $0287         ; colour under cursor
HIBASE  = $0288         ; screen memory page
XMAX    = $0289         ; maximum keyboard buffer size
RPTFLG  = $028A         ; key repeat. $80 = repeat all, $40 = repeat none,
                    ; $00 = repeat cursor movement keys, insert/delete
                    ; key and the space bar
KOUNT   = $028B         ; repeat speed counter
DELAY   = $028C         ; repeat delay counter

; This flag signals which of the SHIFT, CTRL, or C= keys are currently being pressed.

; A value of $01 signifies that one of the SHIFT keys is being pressed, a $02 shows that
; the C= key is down, and $04 means that the CTRL key is being pressed. If more than one
; key is held down, these values will be added e.g. $03 indicates that SHIFT and C= are
; both held down.

; Pressing the SHIFT and C= keys at the same time will toggle the character set that is
; presently being used between the uppercase/graphics set, and the lowercase/uppercase
; set.

; While this changes the appearance of all of the characters on the screen at once it
; has nothing whatever to do with the keyboard shift tables and should not be confused
; with the printing of SHIFTed characters, which affects only one character at a time.

SHFLAG  = $028D         ; keyboard shift/control flag
                    ; bit   key(s) 1 = down
                    ; ---   ---------------
                    ; 7-3   unused
                    ;  2    CTRL
                    ;  1    C=
                    ;  0    SHIFT

; This location, in combination with the one above, is used to debounce the special
; SHIFT keys. This will keep the SHIFT/C= combination from changing character sets
; back and forth during a single pressing of both keys.

LSTSHF  = $028E         ; SHIFT/CTRL/C= keypress last pattern

; This location points to the address of the Operating System routine which actually
; determines which keyboard matrix lookup table will be used.

; The routine looks at the value of the SHIFT flag at SHFLAG, and based on what value
; it finds there, stores the address of the correct table to use at location KEYTAB.

KEYLOG  = $028F         ; keyboard decode logic pointer low byte
;     $0290         ; keyboard decode logic pointer high byte

; This flag is used to enable or disable the feature which lets you switch between the
; uppercase/graphics and upper/lowercase character sets by pressing the SHIFT and
; Commodore logo keys simultaneously.

MODE    = $0291         ; shift mode switch, $00 = enabled, $80 = locked

; This location is used to determine whether moving the cursor past the ??xx column of
; a logical line will cause another physical line to be added to the logical line.

; A value of 0 enables the screen to scroll the following lines down in order to add
; that line; any nonzero value will disable the scroll.

; This flag is set to disable the scroll temporarily when there are characters waiting
; in the keyboard buffer, these may include cursor movement characters that would
; eliminate the need for a scroll.

AUTODN  = $0292         ; screen scrolling flag, $00 = enabled

M51CTR  = $0293         ; pseudo 6551 control register. the first character of
                    ; the OPEN RS-232 filename will be stored here
                    ; bit   function
                    ; ---   --------
                    ;  7    2 stop bits/1 stop bit
                    ; 65    word length
                    ; ---   -----------
                    ; 00    8 bits
                    ; 01    7 bits
                    ; 10    6 bits
                    ; 11    5 bits
                    ;  4    unused
                    ; 3210  baud rate
                    ; ----  ---------
                    ; 0000  user rate *
                    ; 0001     50
                    ; 0010     75
                    ; 0011    110
                    ; 0100    134.5
                    ; 0101    150
                    ; 0110    300
                    ; 0111    600
                    ; 1000   1200
                    ; 1001   1800
                    ; 1010   2400
                    ; 1011   3600
                    ; 1100   4800 *
                    ; 1101   7200 *
                    ; 1110   9600 *
                    ; 1111  19200 * * = not implemented
M51CDR  = $0294         ; pseudo 6551 command register. the second character of
                    ; the OPEN RS-232 filename will be stored here
                    ; bit   function
                    ; ---   --------
                    ; 765   parity
                    ; ---   ------
                    ; xx0   disabled
                    ; 001   odd
                    ; 011   even
                    ; 101   mark
                    ; 111   space
                    ;  4    duplex half/full
                    ;  3    unused
                    ;  2    unused
                    ;  1    unused
                    ;  0    handshake - X line/3 line
;LAB_0295   = $0295     ; Nonstandard Bit Timing low byte. the third character
                    ; of the OPEN RS-232 filename will be stored here
;LAB_0296   = $0296     ; Nonstandard Bit Timing high byte. the fourth character
                    ; of the OPEN RS-232 filename will be stored here
RSSTAT  = $0297         ; RS-232 status register
                    ; bit   function
                    ; ---   --------
                    ;  7    break
                    ;  6    no DSR detected
                    ;  5    unused
                    ;  4    no CTS detected
                    ;  3    unused
                    ;  2    Rx buffer overrun
                    ;  1    framing error
                    ;  0    parity error
BITNUM  = $0298         ; number of bits to be sent/received
BAUDOF  = $0299         ; time of one bit cell low byte
;     $029A         ; time of one bit cell high byte
RIDBE   = $029B         ; index to Rx buffer end
RIDBS   = $029C         ; index to Rx buffer start
RODBS   = $029D         ; index to Tx buffer start
RODBE   = $029E         ; index to Tx buffer end
IRQTMP  = $029F         ; saved IRQ low byte
;     $02A0         ; saved IRQ high byte

; $02A1 to $02FF - unused

IERROR  = $0300         ; BASIC vector - print error message
IMAIN   = $0302         ; BASIC vector - main command processor
ICRNCH  = $0304         ; BASIC vector - tokenise keywords
IQPLOP  = $0306         ; BASIC vector - list program
IGONE   = $0308         ; BASIC vector - execute next command
IEVAL   = $030A         ; BASIC vector - get value from line

; Before every SYS command each of the registers is loaded with the value found in the
; corresponding storage address. Upon returning to BASIC with an RTS instruction, the new
; value of each register is stored in the appropriate storage address.

; This feature allows you to place the necessary values into the registers from BASIC
; before you SYS to a KERNAL or BASIC ML routine. It also enables you to examine the
; resulting effect of the routine on the registers, and to preserve the condition of the
; registers on exit for subsequent SYS calls.

SAREG   = $030C         ; .A for SYS command
SXREG   = $030D         ; .X for SYS command
SYREG   = $030E         ; .Y for SYS command
SPREG   = $030F         ; .P for SYS command

; $0311 to $0313 - unused

CINV    = $0314         ; IRQ vector
CBINV   = $0316         ; BRK vector
NMINV   = $0318         ; NMI vector

IOPEN   = $031A         ; KERNAL vector - open a logical file
ICLOSE  = $031C         ; KERNAL vector - close a specified logical file
ICHKIN  = $031E         ; KERNAL vector - open channel for input
ICKOUT  = $0320         ; KERNAL vector - open channel for output
ICLRCN  = $0322         ; KERNAL vector - close input and output channels
IBASIN  = $0324         ; KERNAL vector - input character from channel
IBSOUT  = $0326         ; KERNAL vector - output character to channel
ISTOP   = $0328         ; KERNAL vector - scan stop key
IGETIN  = $032A         ; KERNAL vector - get character from keyboard queue
ICLALL  = $032C         ; KERNAL vector - close all channels and files
USRCMD  = $032E         ; User vector

ILOAD   = $0330         ; KERNAL vector - load
ISAVE   = $0332         ; KERNAL vector - save

TBUFFR  = $033C         ; to $03FB - cassette buffer

; $03FC to $03FF - unused


;***********************************************************************************;
;
; hardware equates

VICCR0  = $9000         ; screen origin - horizontal
                    ; bit   function
                    ; ---   --------
                    ;  7    interlace mode (NTSC only)
                    ; 6-0   horizontal origin
VICCR1  = $9001         ; screen origin - vertical
VICCR2  = $9002         ; video address and screen columns
                    ; bit   function
                    ; ---   --------
                    ;  7    video memory address va9
                    ;   colour memory address va9
                    ; 6-0   number of columns
VICCR3  = $9003         ; screen rows and character height
                    ; bit   function
                    ; ---   --------
                    ;  7    raster line b0
                    ; 6-1   number of rows
                    ;  0    character height (8/16 bits)
VICCR4  = $9004         ; raster line b8-b1
VICCR5  = $9005         ; video and character memory addresses
                    ; bit   function
                    ; ---   --------
                    ; 7-4   video memory address va13-va10
                    ; 3-0   character memory address va13-va10

                    ; 0000 ROM  $8000   set 1
                    ; 0001  "   $8400
                    ; 0010  "   $8800   set 2
                    ; 0011  "   $8C00
                    ; 1100 RAM  $1000
                    ; 1101  "   $1400
                    ; 1110  "   $1800
                    ; 1111  "   $1C00
VICCR6  = $9006         ; light pen horizontal position
VICCR7  = $9007         ; light pen vertical position
VICCR8  = $9008         ; paddle X
VICCR9  = $9009         ; paddle Y
VICCRA  = $900A         ; oscillator 1
                    ; bit   function
                    ; ---   --------
                    ;  7    enable
                    ; 6-0   frequency
VICCRB  = $900B         ; oscillator 2
                    ; bit   function
                    ; ---   --------
                    ;  7    enable
                    ; 6-0   frequency
VICCRC  = $900C         ; oscillator 3
                    ; bit   function
                    ; ---   --------
                    ;  7    enable
                    ; 6-0   frequency
VICCRD  = $900D         ; white noise
                    ; bit   function
                    ; ---   --------
                    ;  7    enable
                    ; 6-0   frequency
VICCRE  = $900E         ; auxiliary colour and volume
                    ; bit   function
                    ; ---   --------
                    ; 7-4   auxiliary colour
                    ; 3-0   volume
VICCRF  = $900F         ; background and border colour
                    ; bit   function
                    ; ---   --------
                    ; 7-4   background colour
                    ;  3    reverse video
                    ; 2-0   border colour

VIA1PB      = $9110     ; VIA 1 DRB
                    ; bit   function
                    ; ---   --------
                    ;  7    DSR in
                    ;  6    CTS in
                    ;  5    unused
                    ;  4    DCD in
                    ;  3    RI in
                    ;  2    DTR out
                    ;  1    RTS out
                    ;  0    data in
VIA1PA1     = $9111     ; VIA 1 DRA
                    ; bit   function
                    ; ---   --------
                    ;  7    serial ATN out
                    ;  6    cassette switch
                    ;  5    light pen
                    ;  4    joy 2
                    ;  3    joy 1
                    ;  2    joy 0
                    ;  1    serial DATA in
                    ;  0    serial CLK in
VIA1DDRB    = $9112     ; VIA 1 DDRB
VIA1DDRA    = $9113     ; VIA 1 DDRA
VIA1T1CL    = $9114     ; VIA 1 T1C_l
VIA1T1CH    = $9115     ; VIA 1 T1C_h
VIA1T2CL    = $9118     ; VIA 1 T2C_l
VIA1T2CH    = $9119     ; VIA 1 T2C_h
VIA1ACR     = $911B     ; VIA 1 ACR
                    ; bit   function
                    ; ---   --------
                    ;  7    T1 PB7 enabled/disabled
                    ;  6    T1 free run/one shot
                    ;  5    T2 clock PB6/ø2
                    ; 432   function
                    ; ---   --------
                    ; 000   shift register disabled
                    ; 001   shift in, rate controlled by T2
                    ; 010   shift in, rate controlled by ø2
                    ; 011   shift in, rate controlled by external clock
                    ; 100   shift out, rate controlled by T2, free run mode
                    ; 101   shift out, rate controlled by T2
                    ; 110   shift out, rate controlled by ø2
                    ; 111   shift out, rate controlled by external clock
                    ;  1    PB latch enabled/disabled
                    ;  0    PA latch enabled/disabled
VIA1PCR     = $911C     ; VIA 1 PCR
                    ; bit   function
                    ; ---   --------
                    ; 765   CB2 control
                    ; ---   -----------
                    ; 000   Interrupt Input Mode
                    ; 001   Independent Interrupt Input Mode
                    ; 010   Input Mode
                    ; 011   Independent Input Mode
                    ; 100   Handshake Output Mode
                    ; 101   Pulse Output Mode
                    ; 110   Manual Output Mode, CB2 low
                    ; 111   Manual Output Mode, CB2 high
                    ;  4    CB1 edge positive/negative
                    ; 321   CA2 control
                    ; ---   -----------
                    ; 000   Interrupt Input Mode
                    ; 001   Independent Interrupt Input Mode
                    ; 010   Input Mode
                    ; 011   Independent Input Mode
                    ; 100   Handshake Output Mode
                    ; 101   Pulse Output Mode
                    ; 110   Manual Output Mode, CA2 low
                    ; 111   Manual Output Mode, CA2 high
                    ;  0    CA1 edge positive/negative

; The status bit is a not normal flag. It goes high if both an interrupt flag in the IFR
; and the corresponding enable bit in the IER are set. It can be cleared only by clearing
; all the active flags in the IFR or disabling all active interrupts in the IER.

VIA1IFR     = $911D     ; VIA 1 IFR
                    ; bit   function        cleared by
                    ; ---   --------        ----------
                    ;  7    interrupt status    clearing all enabled interrupts
                    ;  6    T1 interrupt        read T1C_l, write T1C_h
                    ;  5    T2 interrupt        read T2C_l, write T2C_h
                    ;  4    CB1 transition      read or write port B
                    ;  3    CB2 transition      read or write port B
                    ;  2    8 shifts done       read or write the shift register
                    ;  1    CA1 transition      read or write port A
                    ;  0    CA2 transition      read or write port A

; If enable/disable bit is a zero during a write to this register, each 1 in bits 0-6
; clears the corresponding bit in the IER. If this bit is a one during a write to this
; register, each 1 in bits 0-6 will set the corresponding IER bit.

VIA1IER     = $911E     ; VIA 1 IER
                    ; bit   function
                    ; ---   --------
                    ;  7    enable/disable
                    ;  6    T1 interrupt
                    ;  5    T2 interrupt
                    ;  4    CB1 transition
                    ;  3    CB2 transition
                    ;  2    8 shifts done
                    ;  1    CA1 transition
                    ;  0    CA2 transition

VIA1PA2     = $911F     ; VIA 1 DRA, no handshake
                    ; bit   function
                    ; ---   --------
                    ;  7    ATN out
                    ;  6    cassette switch
                    ;  5    joystick fire, light pen
                    ;  4    joystick left
                    ;  3    joystick down
                    ;  2    joystick up
                    ;  1    serial dat in
                    ;  0    serial clk in

VIA2PB      = $9120     ; VIA 2 DRB, keyboard column drive
VIA2PA1     = $9121     ; VIA 2 DRA, keyboard row port
                    ; VIC 20 keyboard matrix layout
                    ;   c7  c6  c5  c4  c3  c2  c1  c0
                    ;   +----------------------------------------------------------------
                    ; r7|   [F7]    [F5]    [F3]    [F1]    [DWN]   [RGT]   [RET]   [DEL]
                    ; r6|   [HOME]  [UP]    =   [RSH]   /   ;   *   £
                    ; r5|   -   @   :   .   ,   L   P   +
                    ; r4|   0   O   K   M   N   J   I   9
                    ; r3|   8   U   H   B   V   G   Y   7
                    ; r2|   6   T   F   C   X   D   R   5
                    ; r1|   4   E   S   Z   [LSH]   A   W   3
                    ; r0|   2   Q   [C=]    [SP]    [RUN]   [CTL]   [<-]    1

VIA2DDRB    = $9122     ; VIA 2 DDRB
VIA2DDRA    = $9123     ; VIA 2 DDRA
VIA2T1CL    = $9124     ; VIA 2 T1C_l
VIA2T1CH    = $9125     ; VIA 2 T1C_h
VIA2T2CL    = $9128     ; VIA 2 T2C_l
VIA2T2CH    = $9129     ; VIA 2 T2C_h
VIA2ACR     = $912B     ; VIA 2 ACR
VIA2PCR     = $912C     ; VIA 2 PCR

; The status bit is a not normal flag. it goes high if both an interrupt flag in the IFR
; and the corresponding enable bit in the IER are set. It can be cleared only by clearing
; all the active flags in the IFR or disabling all active interrupts in the IER.

VIA2IFR     = $912D     ; VIA 2 IFR
                    ; bit   function        cleared by
                    ; ---   --------        ----------
                    ;  7    interrupt status    clearing all enabled interrupts
                    ;  6    T1 interrupt        read T1C_l, write T1C_h
                    ;  5    T2 interrupt        read T2C_l, write T2C_h
                    ;  4    CB1 transition      read or write port B
                    ;  3    CB2 transition      read or write port B
                    ;  2    8 shifts done       read or write the shift register
                    ;  1    CA1 transition      read or write port A
                    ;  0    CA2 transition      read or write port A

; If enable/disable bit is a zero during a write to this register, each 1 in bits 0-6
; clears the corresponding bit in the IER. If this bit is a one during a write to this
; register, each 1 in bits 0-6 will set the corresponding IER bit.

VIA2IER     = $912E     ; VIA 2 IER
                    ; bit   function
                    ; ---   --------
                    ;  7    enable/disable
                    ;  6    T1 interrupt
                    ;  5    T2 interrupt
                    ;  4    CB1 transition
                    ;  3    CB2 transition
                    ;  2    8 shifts done
                    ;  1    CA1 transition
                    ;  0    CA2 transition

VIA2PA2     = $912F     ; VIA 2 DRA, keyboard row, no handshake

XROMCOLD    = $A000     ; autostart ROM initial entry vector
XROMWARM    = $A002     ; autostart ROM break entry vector
XROMID      = $A004     ; autostart ROM identifier string start


;***********************************************************************************;
;
; BASIC keyword token values. Tokens not used in the source are included for
; completeness.

; command tokens

TK_END      = $80           ; END token
TK_FOR      = $81           ; FOR token
TK_NEXT     = $82           ; NEXT token
TK_DATA     = $83           ; DATA token
TK_INFL     = $84           ; INPUT# token
TK_INPUT    = $85           ; INPUT token
TK_DIM      = $86           ; DIM token
TK_READ     = $87           ; READ token

TK_LET      = $88           ; LET token
TK_GOTO     = $89           ; GOTO token
TK_RUN      = $8A           ; RUN token
TK_IF       = $8B           ; IF token
TK_RESTORE  = $8C           ; RESTORE token
TK_GOSUB    = $8D           ; GOSUB token
TK_RETURN   = $8E           ; RETURN token
TK_REM      = $8F           ; REM token

TK_STOP     = $90           ; STOP token
TK_ON       = $91           ; ON token
TK_WAIT     = $92           ; WAIT token
TK_LOAD     = $93           ; LOAD token
TK_SAVE     = $94           ; SAVE token
TK_VERIFY   = $95           ; VERIFY token
TK_DEF      = $96           ; DEF token
TK_POKE     = $97           ; POKE token

TK_PRINFL   = $98           ; PRINT# token
TK_PRINT    = $99           ; PRINT token
TK_CONT     = $9A           ; CONT token
TK_LIST     = $9B           ; LIST token
TK_CLR      = $9C           ; CLR token
TK_CMD      = $9D           ; CMD token
TK_SYS      = $9E           ; SYS token
TK_OPEN     = $9F           ; OPEN token

TK_CLOSE    = $A0           ; CLOSE token
TK_GET      = $A1           ; GET token
TK_NEW      = $A2           ; NEW token

; secondary keyword tokens

TK_TAB      = $A3           ; TAB( token
TK_TO       = $A4           ; TO token
TK_FN       = $A5           ; FN token
TK_SPC      = $A6           ; SPC( token
TK_THEN     = $A7           ; THEN token

TK_NOT      = $A8           ; NOT token
TK_STEP     = $A9           ; STEP token

; operator tokens

TK_PLUS     = $AA           ; + token
TK_MINUS    = $AB           ; - token
TK_MUL      = $AC           ; * token
TK_DIV      = $AD           ; / token
TK_POWER    = $AE           ; ^ token
TK_AND      = $AF           ; AND token

TK_OR       = $B0           ; OR token
TK_GT       = $B1           ; > token
TK_EQUAL    = $B2           ; = token
TK_LT       = $B3           ; < token

; function tokens

TK_SGN      = $B4           ; SGN token
TK_INT      = $B5           ; INT token
TK_ABS      = $B6           ; ABS token
TK_USR      = $B7           ; USR token

TK_FRE      = $B8           ; FRE token
TK_POS      = $B9           ; POS token
TK_SQR      = $BA           ; SQR token
TK_RND      = $BB           ; RND token
TK_LOG      = $BC           ; LOG token
TK_EXP      = $BD           ; EXP token
TK_COS      = $BE           ; COS token
TK_SIN      = $BF           ; SIN token

TK_TAN      = $C0           ; TAN token
TK_ATN      = $C1           ; ATN token
TK_PEEK     = $C2           ; PEEK token
TK_LEN      = $C3           ; LEN token
TK_STRS     = $C4           ; STR$ token
TK_VAL      = $C5           ; VAL token
TK_ASC      = $C6           ; ASC token
TK_CHRS     = $C7           ; CHR$ token

TK_LEFTS    = $C8           ; LEFT$ token
TK_RIGHTS   = $C9           ; RIGHT$ token
TK_MIDS     = $CA           ; MID$ token
TK_GO       = $CB           ; GO token

TK_PI       = $FF           ; PI token

;***********************************************************************************;
;
; floating point accumulator offsets

FAC_EXPT    = $00
FAC_MANT    = $01
FAC_SIGN    = $05
