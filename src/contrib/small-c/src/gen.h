#ifndef __GEN_H__
#define __GEN_H__

extern char outb (char c);
extern void outw (int v);
extern void outs (char ptr[]);
extern void print_tab (void);
extern void outtabs (char ptr[]);
extern void newline (void);
extern void outl (char ptr[]);
extern void outn (int number);
extern void gen_srcline (char *n);
extern int getlabel (void);
extern void store (LVALUE * lval);
extern int  rvalue (LVALUE * lval, int reg);
extern void test (int label, int ft);
extern void scale_const (int type, int otag, int *size);
extern void gen_divide (LVALUE *, LVALUE *);
extern void gen_modulo (LVALUE *, LVALUE *);
extern void gen_ashiftr (LVALUE *);

#endif // #ifndef __GEN_H__
