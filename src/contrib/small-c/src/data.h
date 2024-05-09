///////////////
/// SYMBOLS ///
///////////////

extern SYMBOL symbol_table[NUMBER_OF_GLOBALS +
                           NUMBER_OF_LOCALS];
extern int global_table_index, rglobal_table_index;
extern int local_table_index;
extern WHILE ws[];

//////////////////////
/// CONTROL BLOCKS ///
//////////////////////

extern int while_table_index;
extern int swstcase[];
extern int swstlab[];
extern int swstp;
extern int iflevel, skiplevel;

/////////////////
/// LITERLALS ///
/////////////////

extern char litq[];
extern int litptr;

//////////////
/// MACROS ///
//////////////

extern char macq[];
extern int macptr;

/////////////
/// INPUT ///
/////////////

extern char line[];
extern char mline[];
extern int lptr, mptr;
// Global input filenames for error
// messages.
extern char finame[INCLSIZ + 1][20];
// Source file line counters for error
// messages.
extern int srcln[];

extern FILE *input, *input2, *output;
extern FILE *inclstk[];
extern int inclsp;
extern char fname[];
extern int *iptr;
extern int errfile;

///////////////
/// STRUCTS ///
///////////////

// start of structure tag table 
extern TAG_SYMBOL tags[NUMTAG];
// ptr to next entry 
extern int tag;

// structure member table 
extern SYMBOL members[NUMMEMB];
// ptr to next member< 
extern int member;

// miscellaneous storage 
extern int nxtlab,
    litlab, stkp, ncmp, errcnt,
    glbflag, ctext, cmode, lastst;

extern char quote[];
extern int current_symbol_table_idx;
extern int fexitlab;
extern int errs;

///////////////////////
/// INITIALIZATIONS ///
///////////////////////

extern INITIALS initials_table[NUMBER_OF_GLOBALS];

// 5kB space for initialisation data
extern char initials_data_table[INITIALS_SIZE];
extern int initials_idx, initials_data_idx;

////////////////////
/// i8080 TARGET ///
////////////////////

#ifdef I8080
// undocumented 8085 instructions 
extern int uflag;
#endif
