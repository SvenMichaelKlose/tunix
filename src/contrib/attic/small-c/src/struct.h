#ifndef _STRUCT_H_
#define _STRUCT_H_

extern int    find_tag      (char *sname);
extern SYMBOL *find_member  (TAG_SYMBOL * tag, char *sname);
extern SYMBOL *add_member   (char *sname, char identity, char type, int struct_size);
extern int    define_struct (char *sname, int storage, int is_struct);

#endif // #ifndef _STRUCT_H_
