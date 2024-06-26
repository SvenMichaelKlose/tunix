#ifndef _EXPR_H_
#define _EXPR_H_

extern int nosign (LVALUE * is);
extern void expression (int comma); // Assignment
extern int hier1 (LVALUE * lval); // ? : expression
extern int hier1a (LVALUE * lval); // "||"
extern int hier1b (LVALUE * lval); // "&&"
extern int hier1c (LVALUE * lval); // "|"
extern int hier1d (char, LVALUE * lval, int); // "|"
extern int hier2 (LVALUE * lval); // "^"
extern int hier3 (LVALUE * lval); // "&"
extern int hier4 (LVALUE * lval); // "==" and "!="
extern int hier5 (LVALUE * lval); // Other than "==" and "!=".
extern int hier6 (LVALUE * lval); // "<<" and ">>"
extern int hier7 (LVALUE * lval); // "+" and "-"
extern int hier8 (LVALUE * lval); // "*", "/" and "%"
extern int hier9 (LVALUE * lval); // "++", "--" and unary "-"
extern int hier10 (LVALUE * lval); // Array subscript
extern int hier11 (LVALUE * lval);

#endif // #ifndef _EXPR_H_
