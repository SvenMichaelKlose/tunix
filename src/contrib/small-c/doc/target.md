Small C Targets (v3.2)
======================

# Adding New Targets

Adding support for new targets merely
involves creating a new code generator
from an existing one to print code for
an assembler (or whatever else) of your
choice.

## Code Generator Predefinitions

Define ASNM and LDNM to the names of
the assembler and linker respectively

INTSIZE is the size of an integer in
the target machine.
BYTEOFF is the offset of an byte within
an integer on the target machine. (ie:
8080,pdp11 = 0, 6809 = 1, 360 = 3)

## Compiler Architecture

Small-C is a one-pass compiler which
does preprocessing, parsing and
compiling in one go.

Internally the intermediate
representation is a two-adress machine
using two int registers instead of
addresses.  One register is called the
'primary register' as it always holds
the first operand and the result of an
operation.  The 'secondary' register
serves holding an optional second
operand or as temporary storage for
intermediate results.

Local variables (and function arguments
alongside) are stored on the CPU stack.
All arguments are of size 'int'.

Code generators need to provide 15
well-defined functions to be complete.
gen_add_const (int val)
gen_add (int *lval, int *lval2)
gen_and ()
gen_asl ()
gen_asr ()
gen_bss ()
gen_code_segment ()
gen_comment ()
gen_complement ()
gen_datab ()
gen_data_segment ()
gen_dataw ()
gen_decl_fun (SYMBOL * scptr)
gen_decl_var (SYMBOL * scptr)
gen_div ()
gen_div2 ()
gen_eq ()
gen_get_indirect (char typeobj, int reg)
gen_get_local (SYMBOL * sym)
gen_get_memory (SYMBOL * sym)
gen_gt ()
gen_gte ()
gen_jump_case ()
gen_jump (int label)
gen_load_1st ()
gen_local (label)
gen_lsr ()
gen_lt ()
gen_lte ()
gen_mod ()
gen_modify_stack (int newstkp)
gen_mul ()
gen_mul2 ()
gen_mul2 ()
gen_mul_const (int type, int size)
gen_neg ()
gen_neq ()
gen_not ()
gen_or ()
gen_pop ()
gen_ptrdec (LVALUE * lval)
gen_ptrinc (LVALUE * lval)
gen_push (HL_REG);
gen_push (int reg)
gen_put_indirect (char typeobj)
gen_put_memory (SYMBOL * sym)
gen_ret ()
gen_sub ()
gen_swap ()
gen_swap_stack ()
gen_test_jump (int label, int ft)
gen_tobool ()
gen_udiv ()
gen_udiv ();
gen_ugt ()
gen_ugte ()
gen_ult ()
gen_ulte ()
gen_umod ()
gen_xor ()
