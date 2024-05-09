#ifndef _PRIMARY_H_
#define _PRIMARY_H_

extern void dosizeof (LVALUE * lval);
extern int primary_local (LVALUE *lval, SYMBOL *symbol);
extern int primary_global (LVALUE *lval, SYMBOL *symbol);
extern int primary_function (LVALUE *lval, char *sname);
extern int primary (LVALUE * lval);
extern int dbltest (LVALUE * val1, LVALUE * val2);
extern void result (LVALUE * lval, LVALUE * lval2);
extern int constant (int val[]);
extern int number (int val[]);
extern int quoted_char (int *value);
extern int quoted_string (int *position);
extern int spechar (void);
extern void callfunction (char *ptr);
extern void needlval (void);

#endif // #ifndef _PRIMARY_H_
