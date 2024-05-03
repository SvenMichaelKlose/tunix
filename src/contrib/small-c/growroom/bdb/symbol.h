#ifndef __BDB_SYMBOL_H__
#define __BDB_SYMBOL_H__

typedef struct _symbol {
    unsigned int  value;
    char          name[1];
} symbol;

extern bdb symdb;

extern int      add_symbol  (char *name, int value);
extern symbol * find_symbol (char *name);
extern void     symbol_init (void);

#endif // #define __BDB_SYMBOL_H__
