struct ircode {
    char *name;
    char param;
};

struct ircode codes[] = {
  {"FILEHEAD",  0},
  {"FILETAIL",  0},
  {"SEG_CODE",  0},
  {"SEG_DATA",  0},
  {"DATAB",     'b'},
  {"DATAW",     'w'},
  {"BSSB",      'b'},
  {"DEFLOCAL",  'l'},
  {"DEFGLOBAL", 's'},
  {"LOCAL",     's'},
  {"GLOBAL",    's'},
  {"IMPORT",    's'},
  {"EXPORT",    's'},

  {"SWAP",      0},

  // Load address of symbol.
  {"LDACI",     'w'},
  {"LDBCI",     'w'},

  // Load memory at symbol.
  {"LDAMC",     's'},
  {"LDAMI",     's'},
  {"LDBMI",     's'},

  // Store primary at address of symbol.
  {"STAC",      's'},
  {"STAI",      's'},

  // Load memory at address in primary
  // register into the primary.
  {"GETC",      0},
  {"GETUC",     0},
  {"GETI",      0},

  // Store primary register at address
  // the secondary register.
  {"PUTC",      0},
  {"PUTI",      0},

  ///////////////////
  /// ARITHMETICS ///
  ///////////////////
  {"ADDSP",     0},
  {"ADDA",      0},
  {"ADDB",      0},
  {"SUB",       0},
  {"NEG",       0},

  ///////////////////////////
  /// INCREMENT/DECREMENT ///
  ///////////////////////////
  {"INCA",      0},
  {"DECA",      0},
  {"DECB",      0},
  {"INCS1",     0},
  {"INCS2",     0},
  {"DECS1",     0},
  {"DECS2",     0},

  ///////////////////////
  /// TYPE CONVERSION ///
  ///////////////////////
  {"SIGNEXT",   0},
  {"BOOL",      0},
  {"NOT",       0},

  /////////////
  /// STACK ///
  /////////////
  {"PUSHA",     0},
  {"PUSHB",     0},
  {"POPA",      0},
  {"POPB",      0},
  {"SWAPSTACK", 0},
  {"SPHL",      0},

  ///////////////////
  /// SUBROUTINES ///
  ///////////////////
  {"CALL",      0},
  {"CALLPTR",   0},
  {"RET",       0},

  //////////////
  /// SHIFTS ///
  //////////////
  {"ASLA",      0},
  {"ASL",       0},
  {"ASR",       0},
  {"LSR",       0},
  {"MUL",       0},
  {"DIV",       0},
  {"UDIV",      0},

  //////////////////////
  /// BIT OPERATIONS ///
  //////////////////////
  {"AND",       0},
  {"OR",        0},
  {"XOR",       0},
  {"COMPA",     0},
  {"COMPB",     0},

  /////////////
  /// JUMPS ///
  /////////////
  {"JMP",       'l'},
  {"JMPNZ",     'l'},
  {"JMPZ",      'l'},
  {"JMPCASE",   'l'},

  /////////////////////////
  /// CONDITIONAL JUMPS ///
  /////////////////////////
  {"EQ",        0},
  {"NE",        0},
  {"LT",        0},
  {"LTE",       0},
  {"GT",        0},
  {"GTE",       0},
  {"ULT",       0},
  {"ULTE",      0},
  {"UGT",       0},
  {"UGTE",      0},
  {"SRCLINE",   0},
  {NULL,        0}
};
