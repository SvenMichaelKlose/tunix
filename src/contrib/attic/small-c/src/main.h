#ifndef __MAIN_H__
#define __MAIN_H__

extern void oputs (char *str);
extern int main (int argc, char *argv[]);
extern void compile (char *file);
extern void frontend_version ();
extern void usage ();
extern void parse ();
extern int do_declarations (int stclass, TAG_SYMBOL * mtag, int is_struct);
extern void dumplits ();
extern void dumpglbs ();
extern void dump_struct (SYMBOL * symbol, int position);
extern void errorsummary ();
extern char filename_typeof (char *s);

#endif // #ifndef __MAIN_H__
