#ifndef INGLE_H
#define INGLE_H

#define WRAP(function_name, bank_name)  function_name

extern void __fastcall__ save_state (unsigned int restart_addr);
extern void __fastcall__ launch (unsigned start, unsigned size);
extern void copy_bank (void);

#endif /* #ifndef INGLE_H */
