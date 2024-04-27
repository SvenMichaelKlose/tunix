#ifndef __IR_H__
#define __IR_H__

extern void initmac (void);
extern void header (void);
extern void trailer (void);
extern void def_local (int);
extern void def_global (char *);
extern int gen_get_local (SYMBOL *);
extern void gen_code_segment ();
extern void gen_data_segment ();
extern void gen_srcline (char *);
extern void gen_comment (void);
extern void gen_datab (void);
extern void gen_dataw (void);
extern void gen_bss (void);
extern void gen_jump (int);
extern void gen_jump_case (void);
extern void gen_call (char *);
extern void gen_test_jump (int lab, int ft);
extern void gen_ret (void);
extern int gen_modify_stack (int);
extern void gen_swap_stack (void);
extern void callstk (void);
extern void gen_test_jump (int lab, int);
extern void gen_local (int);
extern void gen_global (char *);
extern void gen_ldaci (int);
extern void gen_ldacig (char *);
extern void gen_put_indirect (int);
extern void gen_push (int reg);
extern void gen_pop (void);
extern void gen_add_const (int);
extern void gen_mul_const (int type, int);
extern void gen_add (LVALUE *, LVALUE *);
extern void gen_sub (void);
extern void gen_mul (void);
extern void gen_div (void);
extern void gen_udiv (void);
extern void gen_mod (void);
extern void gen_umod (void);
extern void gen_or (void);
extern void gen_and (void);
extern void gen_xor (void);
extern void gen_asl (void);
extern void gen_asr (void);
extern void gen_lsr (void);
extern void gen_swap (void);
extern void gen_not (void);
extern void gen_complement (void);
extern void gen_neg (void);
extern void gen_ptrinc (LVALUE *);
extern void gen_ptrdec (LVALUE *);
extern void gen_div2 (void);
extern void gen_eq (void);
extern void gen_neq (void);
extern void gen_lt (void);
extern void gen_gt (void);
extern void gen_lte (void);
extern void gen_gte (void);
extern void gen_ult (void);
extern void gen_ugt (void);
extern void gen_ulte (void);
extern void gen_ugte (void);
extern void gen_tobool (void);

// Output the variable symbol at scptr
// as an extrn or a public.
extern void gen_decl_var (SYMBOL * scptr);

// Output function symbol at scptr as
// an extrn or a public.
extern void gen_decl_fun (SYMBOL * scptr);

// Load static memory cell into the
// primary register.
extern void gen_get_memory (SYMBOL * sym);

// Load object type indirect through the
// primary into the primary register.
extern void gen_get_indirect (char typeobj, int reg);

// Write primary to static memory cell.
extern void gen_put_memory (SYMBOL * sym);

#endif // #ifndef __IR_H__
