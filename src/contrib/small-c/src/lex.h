#ifndef __LEX_H__
#define __LEX_H__

extern char alpha (char c);
extern char numeric (char c);
extern char alphanumeric (char c);
extern void need_semicolon (void);
extern void junk (void);
extern int endst (void);
extern void needbrack (char *str);
extern void blanks (void);
extern int sstreq (char *str1);
extern int streq (char *str1, char *str2);
extern int astreq (char *str1, char *str2, int len);
extern int match (char *lit);
extern int amatch (char *lit, int len);
extern int get_type (void);

#endif // #ifndef __LEX_H__
