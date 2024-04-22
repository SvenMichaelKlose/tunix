#ifndef SMALLC

// Output the variable symbol at scptr
// as an extrn or a public.
void gen_decl_var (SYMBOL * scptr);

// Output function symbol at scptr as
// an extrn or a public.
void gen_decl_fun (SYMBOL * scptr);

// Load static memory cell into the
// primary register.
void gen_get_memory (SYMBOL * sym);

// Load object type indirect through the
// primary into the primary register.
void gen_get_indirect (char typeobj, int reg);

// Write primary to static memory cell.
void gen_put_memory (SYMBOL * sym);

#endif
