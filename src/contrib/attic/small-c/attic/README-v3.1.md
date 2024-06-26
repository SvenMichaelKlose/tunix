Revived version of SmallC based on Chris
Lewis' port to UNIX V.  I have mainly
rewritten code to use structures and to
make it compile using GCC silently
without warnigns.

Support for one line comments from C99
specification was added as well as
capability to handle Windows EOLs.

Initialisation of global variables is
also possible. When not initialised
global var is assigned zero at compile
time.

Furthermore support for ANSI style
method declaration, support for unsigned
types, support for undocumented 8085
istructions LHLX, SHLX, LDSI, ARHL and
support for structs and unions have
been added.

Generated code is suitable for ASXXXX
assembler/linker.

SmallC compiled for win32 can be
downloaded here:
https://drive.google.com/file/d/0B2TmWnRjWCj2alRqZHM1VEgwNFE/edit?usp=sharing
