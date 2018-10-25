#ifndef BANK_ALLOCATOR_H
#define BANK_ALLOCATOR_H

extern void init_bank_allocator (void);
extern char alloc_bank ();
extern void __fastcall__ free_bank (char);

#endif /* #ifndef BANK_ALLOCATOR_H */
