#ifndef INGLE_H
#define INGLE_H

#define WRAP(function_name, bank_name)  function_name

#define INGLE_FULL_STATE_COPY   1

extern void __fastcall__ save_state (unsigned int restart_addr, char flags);
extern void __fastcall__ launch (unsigned long offset, unsigned start, unsigned size);
extern void copy_bank (void);

#endif /* #ifndef INGLE_H */
