
// storage words 
extern SYMBOL symbol_table[NUMBER_OF_GLOBALS +
                           NUMBER_OF_LOCALS];
extern int global_table_index, rglobal_table_index;
extern int local_table_index;
extern WHILE ws[];
extern int while_table_index;
extern int swstcase[];
extern int swstlab[];
extern int swstp;
extern char litq[];
extern int litptr;
extern char macq[];
extern int macptr;
extern char line[];
extern char mline[];
extern int lptr, mptr;
// Global input filenames for error
// messages.
extern char finame[INCLSIZ + 1][20];
// Source file line counters for error
// messages.
extern int srcln[];

// start of structure tag table 
extern TAG_SYMBOL tag_table[NUMTAG];
// ptr to next entry 
extern int tag_table_index;

// structure member table 
extern SYMBOL member_table[NUMMEMB];
// ptr to next member< 
extern int member_table_index;

// miscellaneous storage 
extern int nxtlab,
    litlab, stkp, argstk, ncmp, errcnt,
    glbflag, ctext, cmode, lastst;

extern FILE *input, *input2, *output;
extern FILE *inclstk[];
extern int inclsp;
extern char fname[];

extern char quote[];
extern int current_symbol_table_idx;
extern int *iptr;
extern int fexitlab;
extern int iflevel, skiplevel;
extern int errfile;
extern int sflag;
extern int cflag;
extern int errs;
extern int aflag;

#ifdef I8080
// undocumented 8085 instructions 
extern int uflag;
#endif

extern INITIALS initials_table[NUMBER_OF_GLOBALS];

// 5kB space for initialisation data
extern char initials_data_table[INITIALS_SIZE];
extern int initials_idx, initials_data_idx;
