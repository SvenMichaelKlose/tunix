// Intel 8080 architecture defs 
#define INTSIZE 2

// miscellaneous 
#define FOREVER for(;;)
#define FALSE   0
#define TRUE    1
#define NO      0
#define YES     1

#define EOS     0
#define LF      10
#define BKSP    8
#define CR      13
#define FFEED   12
#define TAB     9

// system-wide name size (for symbols) 
#define NAMESIZE        33
#define NAMEMAX         32

struct symbol {
    // symbol name 
    char name[NAMESIZE];
    // variable, array, pointer,
    // function 
    int identity;
    // char, int, uchar, unit 
    int type;
    // public, auto, extern, static
    // lstatic, defauto 
    int storage;
    int offset;
    // index of struct in tag table 
    int tagidx;
    // The size, in bytes, of a member
    // of a struct - only used for
    // member declarations.
    int struct_size;
};
#define SYMBOL struct symbol

#define NUMBER_OF_GLOBALS 100
#define NUMBER_OF_LOCALS 20

// Define the structure tag table parameters 
#define NUMTAG      10

struct tag_symbol {
    // structure tag name 
    char name[NAMESIZE];
    // size of struct in bytes 
    int size;
    // index of first member 
    int member_idx;
    // number of tag members 
    int number_of_members;
};
#define TAG_SYMBOL struct tag_symbol

#ifdef SMALL_C
#define NULL_TAG 0
#else
#define NULL_TAG (TAG_SYMBOL *)0
#endif

// Define the structure member table
// parameters 
#define NUMMEMB     30

// Possible entries for "ident".
#define VARIABLE        1
#define ARRAY           2
#define POINTER         3
#define FUNCTION        4

// Possible entries for "type"
// High order 14 bits give length of
// object.
// Low order 2 bits make type unique
// within length.
#define UNSIGNED    1
#define STRUCT      2
#define CCHAR       (1 << 2)
#define UCHAR       ((1 << 2) + 1)
#define CINT        (2 << 2)
#define UINT        ((2 << 2) + 1)

// Possible entries for "storage".
#define PUBLIC  1
#define AUTO    2
#define EXTERN  3

#define STATIC  4
#define LSTATIC 5
#define DEFAUTO 6

// "do"/"for"/"while"/"switch"
// statement stack 
#define WSTABSZ 20

struct while_rec {
    // symbol table address 
    int symbol_idx;
    // stack pointer 
    int stack_pointer;
    // type 
    int type;
    // case or test 
    int case_test;
    // continue label ? 
    int incr_def;
    // body of loop, switch ? 
    int body_tab;
    // exit label 
    int while_exit;
};
#define WHILE struct while_rec

// possible entries for "wstyp" 
#define WSWHILE 0
#define WSFOR   1
#define WSDO    2
#define WSSWITCH        3

// "switch" label stack 
#define SWSTSZ  100

// literal pool 
#define LITABSZ 5000
#define LITMAX  LITABSZ-1

// input line 
#define LINESIZE        150
#define LINEMAX (LINESIZE-1)
#define MPMAX   LINEMAX

// macro (define) pool 
#define MACQSIZE        1500
#define MACMAX  (MACQSIZE-1)

// "include" stack 
#define INCLSIZ     3

// statement types (tokens) 
#define STIF        1
#define STWHILE     2
#define STRETURN    3
#define STBREAK     4
#define STCONT      5
#define STASM       6
#define STEXP       7
#define STDO        8
#define STFOR       9
#define STSWITCH    10

#define DEFLIB  inclib()

#define FETCH  1
#define HL_REG 1<<1
#define DE_REG 1<<2

struct lvalue {
    // Symbol table address, or
    // 0 for constant.
    SYMBOL *symbol;
    // Type of indirect object,
    // 0 for static object.
    int indirect;
    // Type of pointer or array,
    // 0 for other idents.
    int ptr_type;
    // Tag symbol address,
    // 0 if not struct.
    TAG_SYMBOL *tagsym;
};
#define LVALUE struct lvalue

// Path to include directories. set at
// compile time on host machine
char *inclib ();

WHILE *readwhile ();
WHILE *findwhile ();
WHILE *readswitch ();

// Output the variable symbol at scptr
// as an extrn or a public.
void ppubext (SYMBOL * scptr);

// Output function symbol at scptr as
// an extrn or a public.
void fpubext (SYMBOL * scptr);

// Load static memory cell into the
// primary register.
void gen_get_memory (SYMBOL * sym);

// Load object type indirect through the
// primary into the primary register.
void gen_get_indirect (char typeobj, int reg);

// Write primary to static memory cell.
void gen_put_memory (SYMBOL * sym);

// Initialisation of global variables-
#define INIT_TYPE       NAMESIZE
#define INIT_LENGTH     NAMESIZE+1
#define INITIALS_SIZE   5*1024

struct initials_table {
    // symbol name 
    char name[NAMESIZE];
    // type 
    int type;
    // length of data (possibly an
    // array) 
    int dim;
    // index of tag or zero 
    int data_len;
};
#define INITIALS struct initials_table

// Determine if 'sname' is a member of
// the struct with tag 'tag'
SYMBOL *find_member (TAG_SYMBOL * tag, char *sname);
