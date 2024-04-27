#ifndef _INITIALS_C_
#define _INITIALS_C_

extern void add_symbol_initials (char *symbol_name, char type);
extern int find_symbol_initials (char *symbol_name);
extern void add_data_initials (char *symbol_name, int type, int value, TAG_SYMBOL * tag);
extern int get_size (char *symbol_name);
extern int get_item_at (char *symbol_name, int position, TAG_SYMBOL * tag);

#endif // #ifndef _INITIALS_C_
