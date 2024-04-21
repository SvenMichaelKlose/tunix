#include <stdio.h>
#include "defs.h"

// storage words 
SYMBOL symbol_table[NUMBER_OF_GLOBALS + NUMBER_OF_LOCALS];
int global_table_index,
    rglobal_table_index;
int local_table_index;

WHILE ws[WSTABSZ];
int while_table_index;

int swstcase[SWSTSZ];
int swstlab[SWSTSZ];
int swstp;
char litq[LITABSZ];
int litptr;
char macq[MACQSIZE];
int macptr;
char line[LINESIZE];
char mline[LINESIZE];
int lptr, mptr;

// start of structure tag table 
TAG_SYMBOL tag_table[NUMTAG];
// ptr to next entry 
int tag_table_index;

// structure member table 
SYMBOL member_table[NUMMEMB];
// ptr to next member 
int member_table_index;

// miscellaneous storage 
int nxtlab,
    litlab,
    stkp,
    argstk, ncmp, errcnt, glbflag, ctext, cmode, lastst;

FILE *input, *input2, *output;
FILE *inclstk[INCLSIZ];
int inclsp;
char fname[20];

int current_symbol_table_idx;
int *iptr;
int fexitlab;
int iflevel, skiplevel;
int errfile;
int errs;

INITIALS initials_table[NUMBER_OF_GLOBALS];
// 5kB space for initialisation data 
char initials_data_table[INITIALS_SIZE];
int initials_idx = 0, initials_data_idx = 0;


#ifdef I8080
// undocumented 8085 instructions 
int uflag;
#endif
