#ifndef _STMT_H_
#define _STMT_H_

extern int statement (int func);
extern int statement_declare (void);
extern int do_local_declares (int stclass);
extern int do_statement (void);
extern int do_compound (int func);
extern int doif (void);
extern int dowhile (void);
extern int dodo (void);
extern int dofor (void);
extern int doswitch (void);
extern int docase (void);
extern int dodefault (void);
extern int doreturn (void);
extern int dobreak (void);
extern int docont (void);
extern int dumpsw (WHILE * ws);

#endif // #define _STMT_H_
