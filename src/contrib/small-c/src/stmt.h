#ifndef _STMT_H_
#define _STMT_H_

extern int statement (int func);
extern int statement_declare (void);
extern int do_local_declares (int stclass);
extern void do_statement (void);
extern void do_compound (int func);
extern void doif (void);
extern void dowhile (void);
extern void dodo (void);
extern void dofor (void);
extern void doswitch (void);
extern void docase (void);
extern void dodefault (void);
extern void doreturn (void);
extern void dobreak (void);
extern void docont (void);
extern void dumpsw (WHILE * ws);

#endif // #define _STMT_H_
