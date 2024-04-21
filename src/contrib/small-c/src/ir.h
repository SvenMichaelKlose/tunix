// TODO: Describe what the code do
// exactly. Format is:
//
// // Description
// #define IR_CODENAM   code // code args
//
// Non-string arguments are always words.
// Strings are zero-terminated.

// Switch to code segment.
#define IR_SEG_CODE     1   // -
// Switch to cdata segment.
#define IR_SEG_DATA     2   // -
#define IR_DATAB        3   // byte
#define IR_DATAW        4   // word
#define IR_BSSB         5   // byte
#define IR_DEFLOCAL     6   // label index
#define IR_DEFGLOBAL    7   // string
#define IR_LOCAL        8   // label index
#define IR_GLOBAL       9   // string
#define IR_IMPORT       10  // string
#define IR_EXPORT       11  // string
#define IR_SWAP         12  // -
#define IR_LDAL         13  // byte
// TODO: Where are the other loads with
// constants?
#define IR_LDA          14
#define IR_LDB          15
// Add stack pointer to primary.
#define IR_ADDSP        16  // -
// Add secondary to primary.
#define IR_ADDA         17
#define IR_ADDB         18
#define IR_INCA         19  // -
#define IR_DECA         20  // -
#define IR_DECB         20  // -
#define IR_SIGNEXT      21  // -
#define IR_STA          22
#define IR_STAL         23
#define IR_PUTCHAR      24
#define IR_PUTINT       25
#define IR_GETCHAR      26
#define IR_GETUCHAR     27
#define IR_GETINT       28
#define IR_PUSHA        29  // -
#define IR_PUSHB        30  // -
#define IR_POPA         31  // -
#define IR_POPB         32  // -
#define IR_SWAPSTACK    33  // -
#define IR_INCS1        34  // -
#define IR_INCS2        35  // -
#define IR_DECS1        36  // -
#define IR_DECS2        37  // -
#define IR_SPHL         38
#define IR_CALL         39
#define IR_CALLPTR      40
#define IR_RET          41  // -
#define IR_JMP          42
#define IR_JMPNZ        43
#define IR_JMPZ         44
#define IR_JMPCASE      45
#define IR_BOOL         46 // -
#define IR_LNEG         47 // -
#define IR_NEG          48 // -
#define IR_ASLA         49
#define IR_ASL          50 // -
#define IR_ASR          51 // -
#define IR_LSR          52 // -
#define IR_ADDA         53
#define IR_ADDB         53
#define IR_SUB          54  // -
#define IR_MUL          55  // -
#define IR_DIV          56  // -
#define IR_UDIV         57  // -
#define IR_AND          58  // -
#define IR_OR           59  // -
#define IR_XOR          61  // -
#define IR_COMPA        62  // -
#define IR_COMPB        63  // -
#define IR_EQ           64  // -
#define IR_NE           65  // -
#define IR_LT           66  // -
#define IR_LTE          67  // -
#define IR_GT           68  // -
#define IR_GTE          69  // -
#define IR_ULT          70  // -
#define IR_ULTE         71  // -
#define IR_UGT          72  // -
#define IR_UGTE         73  // -
#define IR_SRCLINE      74  // -
