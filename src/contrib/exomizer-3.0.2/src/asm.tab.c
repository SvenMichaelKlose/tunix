/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 28 "asm.y"

#include "int.h"
#include "parse.h"
#include "vec.h"
#include "membuf.h"
#include "log.h"
#include <stdio.h>
#define YYERROR_VERBOSE

static struct vec asm_atoms[1];

/* prototypes to silence compiler warnings */
int yylex(void);
void yyerror(const char *s);


#line 88 "asm.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "asm.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_INCLUDE = 3,                    /* INCLUDE  */
  YYSYMBOL_IF = 4,                         /* IF  */
  YYSYMBOL_DEFINED = 5,                    /* DEFINED  */
  YYSYMBOL_MACRO = 6,                      /* MACRO  */
  YYSYMBOL_MACRO_STRING = 7,               /* MACRO_STRING  */
  YYSYMBOL_ORG = 8,                        /* ORG  */
  YYSYMBOL_ERROR = 9,                      /* ERROR  */
  YYSYMBOL_ECHO1 = 10,                     /* ECHO1  */
  YYSYMBOL_INCBIN = 11,                    /* INCBIN  */
  YYSYMBOL_INCLEN = 12,                    /* INCLEN  */
  YYSYMBOL_INCWORD = 13,                   /* INCWORD  */
  YYSYMBOL_RES = 14,                       /* RES  */
  YYSYMBOL_WORD = 15,                      /* WORD  */
  YYSYMBOL_BYTE = 16,                      /* BYTE  */
  YYSYMBOL_LDA = 17,                       /* LDA  */
  YYSYMBOL_LDX = 18,                       /* LDX  */
  YYSYMBOL_LDY = 19,                       /* LDY  */
  YYSYMBOL_STA = 20,                       /* STA  */
  YYSYMBOL_STX = 21,                       /* STX  */
  YYSYMBOL_STY = 22,                       /* STY  */
  YYSYMBOL_AND = 23,                       /* AND  */
  YYSYMBOL_ORA = 24,                       /* ORA  */
  YYSYMBOL_EOR = 25,                       /* EOR  */
  YYSYMBOL_ADC = 26,                       /* ADC  */
  YYSYMBOL_SBC = 27,                       /* SBC  */
  YYSYMBOL_CMP = 28,                       /* CMP  */
  YYSYMBOL_CPX = 29,                       /* CPX  */
  YYSYMBOL_CPY = 30,                       /* CPY  */
  YYSYMBOL_TSX = 31,                       /* TSX  */
  YYSYMBOL_TXS = 32,                       /* TXS  */
  YYSYMBOL_PHA = 33,                       /* PHA  */
  YYSYMBOL_PLA = 34,                       /* PLA  */
  YYSYMBOL_PHP = 35,                       /* PHP  */
  YYSYMBOL_PLP = 36,                       /* PLP  */
  YYSYMBOL_SEI = 37,                       /* SEI  */
  YYSYMBOL_CLI = 38,                       /* CLI  */
  YYSYMBOL_NOP = 39,                       /* NOP  */
  YYSYMBOL_TYA = 40,                       /* TYA  */
  YYSYMBOL_TAY = 41,                       /* TAY  */
  YYSYMBOL_TXA = 42,                       /* TXA  */
  YYSYMBOL_TAX = 43,                       /* TAX  */
  YYSYMBOL_CLC = 44,                       /* CLC  */
  YYSYMBOL_SEC = 45,                       /* SEC  */
  YYSYMBOL_RTS = 46,                       /* RTS  */
  YYSYMBOL_CLV = 47,                       /* CLV  */
  YYSYMBOL_CLD = 48,                       /* CLD  */
  YYSYMBOL_SED = 49,                       /* SED  */
  YYSYMBOL_JSR = 50,                       /* JSR  */
  YYSYMBOL_JMP = 51,                       /* JMP  */
  YYSYMBOL_BEQ = 52,                       /* BEQ  */
  YYSYMBOL_BNE = 53,                       /* BNE  */
  YYSYMBOL_BCC = 54,                       /* BCC  */
  YYSYMBOL_BCS = 55,                       /* BCS  */
  YYSYMBOL_BPL = 56,                       /* BPL  */
  YYSYMBOL_BMI = 57,                       /* BMI  */
  YYSYMBOL_BVC = 58,                       /* BVC  */
  YYSYMBOL_BVS = 59,                       /* BVS  */
  YYSYMBOL_INX = 60,                       /* INX  */
  YYSYMBOL_DEX = 61,                       /* DEX  */
  YYSYMBOL_INY = 62,                       /* INY  */
  YYSYMBOL_DEY = 63,                       /* DEY  */
  YYSYMBOL_INC = 64,                       /* INC  */
  YYSYMBOL_DEC = 65,                       /* DEC  */
  YYSYMBOL_LSR = 66,                       /* LSR  */
  YYSYMBOL_ASL = 67,                       /* ASL  */
  YYSYMBOL_ROR = 68,                       /* ROR  */
  YYSYMBOL_ROL = 69,                       /* ROL  */
  YYSYMBOL_BIT = 70,                       /* BIT  */
  YYSYMBOL_SYMBOL = 71,                    /* SYMBOL  */
  YYSYMBOL_STRING = 72,                    /* STRING  */
  YYSYMBOL_LAND = 73,                      /* LAND  */
  YYSYMBOL_LOR = 74,                       /* LOR  */
  YYSYMBOL_LNOT = 75,                      /* LNOT  */
  YYSYMBOL_LPAREN = 76,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 77,                    /* RPAREN  */
  YYSYMBOL_COMMA = 78,                     /* COMMA  */
  YYSYMBOL_COLON = 79,                     /* COLON  */
  YYSYMBOL_X = 80,                         /* X  */
  YYSYMBOL_Y = 81,                         /* Y  */
  YYSYMBOL_HASH = 82,                      /* HASH  */
  YYSYMBOL_PLUS = 83,                      /* PLUS  */
  YYSYMBOL_MINUS = 84,                     /* MINUS  */
  YYSYMBOL_MULT = 85,                      /* MULT  */
  YYSYMBOL_DIV = 86,                       /* DIV  */
  YYSYMBOL_MOD = 87,                       /* MOD  */
  YYSYMBOL_LT = 88,                        /* LT  */
  YYSYMBOL_GT = 89,                        /* GT  */
  YYSYMBOL_EQ = 90,                        /* EQ  */
  YYSYMBOL_NEQ = 91,                       /* NEQ  */
  YYSYMBOL_ASSIGN = 92,                    /* ASSIGN  */
  YYSYMBOL_GUESS = 93,                     /* GUESS  */
  YYSYMBOL_NUMBER = 94,                    /* NUMBER  */
  YYSYMBOL_vNEG = 95,                      /* vNEG  */
  YYSYMBOL_LABEL = 96,                     /* LABEL  */
  YYSYMBOL_YYACCEPT = 97,                  /* $accept  */
  YYSYMBOL_stmts = 98,                     /* stmts  */
  YYSYMBOL_stmt = 99,                      /* stmt  */
  YYSYMBOL_atom = 100,                     /* atom  */
  YYSYMBOL_exprs = 101,                    /* exprs  */
  YYSYMBOL_op = 102,                       /* op  */
  YYSYMBOL_am_im = 103,                    /* am_im  */
  YYSYMBOL_am_a = 104,                     /* am_a  */
  YYSYMBOL_am_ax = 105,                    /* am_ax  */
  YYSYMBOL_am_ay = 106,                    /* am_ay  */
  YYSYMBOL_am_zp = 107,                    /* am_zp  */
  YYSYMBOL_am_zpx = 108,                   /* am_zpx  */
  YYSYMBOL_am_zpy = 109,                   /* am_zpy  */
  YYSYMBOL_am_ix = 110,                    /* am_ix  */
  YYSYMBOL_am_iy = 111,                    /* am_iy  */
  YYSYMBOL_expr = 112,                     /* expr  */
  YYSYMBOL_lexpr = 113                     /* lexpr  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  221
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   653

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  97
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  17
/* YYNRULES -- Number of rules.  */
#define YYNRULES  201
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  323

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   351


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   185,   185,   185,   186,   187,   188,   189,   190,   191,
     192,   193,   194,   195,   196,   197,   199,   200,   201,   202,
     203,   205,   207,   210,   211,   213,   214,   215,   216,   217,
     218,   219,   220,   222,   223,   224,   225,   226,   228,   229,
     230,   231,   232,   234,   235,   236,   237,   238,   239,   240,
     242,   243,   244,   246,   247,   248,   250,   251,   252,   253,
     254,   255,   256,   257,   259,   260,   261,   262,   263,   264,
     265,   266,   268,   269,   270,   271,   272,   273,   274,   275,
     277,   278,   279,   280,   281,   282,   283,   284,   286,   287,
     288,   289,   290,   291,   292,   293,   295,   296,   297,   298,
     299,   300,   301,   302,   304,   305,   306,   307,   308,   309,
     311,   312,   313,   314,   315,   316,   317,   318,   319,   320,
     321,   322,   323,   324,   325,   326,   327,   328,   329,   331,
     332,   333,   334,   335,   336,   337,   338,   339,   340,   342,
     343,   344,   345,   347,   348,   349,   350,   352,   353,   354,
     355,   357,   358,   359,   360,   361,   363,   364,   365,   366,
     367,   369,   370,   371,   372,   373,   375,   376,   377,   378,
     379,   381,   382,   384,   385,   386,   387,   388,   389,   390,
     391,   392,   394,   395,   396,   397,   398,   399,   400,   401,
     402,   404,   405,   407,   408,   409,   410,   411,   412,   413,
     414,   416
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "INCLUDE", "IF",
  "DEFINED", "MACRO", "MACRO_STRING", "ORG", "ERROR", "ECHO1", "INCBIN",
  "INCLEN", "INCWORD", "RES", "WORD", "BYTE", "LDA", "LDX", "LDY", "STA",
  "STX", "STY", "AND", "ORA", "EOR", "ADC", "SBC", "CMP", "CPX", "CPY",
  "TSX", "TXS", "PHA", "PLA", "PHP", "PLP", "SEI", "CLI", "NOP", "TYA",
  "TAY", "TXA", "TAX", "CLC", "SEC", "RTS", "CLV", "CLD", "SED", "JSR",
  "JMP", "BEQ", "BNE", "BCC", "BCS", "BPL", "BMI", "BVC", "BVS", "INX",
  "DEX", "INY", "DEY", "INC", "DEC", "LSR", "ASL", "ROR", "ROL", "BIT",
  "SYMBOL", "STRING", "LAND", "LOR", "LNOT", "LPAREN", "RPAREN", "COMMA",
  "COLON", "X", "Y", "HASH", "PLUS", "MINUS", "MULT", "DIV", "MOD", "LT",
  "GT", "EQ", "NEQ", "ASSIGN", "GUESS", "NUMBER", "vNEG", "LABEL",
  "$accept", "stmts", "stmt", "atom", "exprs", "op", "am_im", "am_a",
  "am_ax", "am_ay", "am_zp", "am_zpx", "am_zpy", "am_ix", "am_iy", "expr",
  "lexpr", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-214)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     290,   -71,   -16,   -14,  -214,   -10,    -4,    15,    82,    92,
     101,   103,   186,   189,   350,   369,   372,   383,   186,   186,
     186,   186,   186,   186,   353,   353,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   148,  -214,  -214,  -214,  -214,   383,
     383,   383,   383,   383,   383,   394,   -35,    83,    86,  -214,
    -214,  -214,   109,   154,   122,   148,   123,   128,   132,   148,
     148,   148,   121,   129,  -214,   148,   148,   148,   148,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,   125,   148,
     148,  -214,  -214,  -214,  -214,  -214,   508,  -214,  -214,  -214,
    -214,  -214,   518,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,   -68,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,   148,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,   148,   148,
    -214,  -214,  -214,   138,   140,   154,   154,   429,   -18,   141,
     325,   143,   -48,   -13,   528,    -7,   -68,     7,   145,   149,
     201,   -68,  -214,   538,   111,   148,   148,   148,   148,   148,
     477,   548,   142,   146,   -68,   -68,   -68,  -214,   156,  -214,
     -63,   116,   148,   148,   148,   148,   154,   154,  -214,  -214,
    -214,  -214,  -214,   148,  -214,   148,   148,  -214,   148,  -214,
     162,   144,   150,   160,   161,  -214,  -214,   -41,   -41,  -214,
    -214,  -214,  -214,   163,   166,  -214,   -68,   -68,   -68,   -68,
    -214,   158,    21,   314,   482,   -68,  -214,   148,   164,   169,
    -214,  -214,  -214,  -214,  -214,   148,  -214,   493,  -214,  -214,
     498,  -214,  -214
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,    15,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,   110,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   139,   140,   141,   142,     0,
       0,   151,   156,   161,   166,     0,     0,     0,     0,     3,
      14,    16,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   192,     0,     0,     0,     0,   191,
      25,    28,    29,    30,    26,    27,    31,    32,   174,     0,
       0,    33,    36,    37,    34,    35,   174,    38,    41,    42,
      39,    40,   174,    45,    46,    47,    43,    44,    48,    49,
      52,    50,    51,   174,    55,    53,    54,    56,    59,    60,
      61,    57,    58,    62,    63,    64,    67,    68,    69,    65,
      66,    70,    71,    72,    75,    76,    77,    73,    74,    78,
      79,    80,    83,    84,    85,    81,    82,    86,    87,    88,
      91,    92,    93,    89,    90,    94,    95,    96,    99,   100,
     101,    97,    98,   102,   103,     0,   104,   106,   105,   107,
     109,   108,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   145,   146,   143,   144,   149,   150,   147,   148,
     154,   155,   152,   153,   159,   160,   157,   158,   164,   165,
     162,   163,   169,   170,   167,   168,   172,   171,     0,     0,
       4,     1,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    24,     0,     0,     0,
       0,   173,   187,   177,     0,     0,     0,     0,     0,     0,
       0,   177,     0,     0,   177,     5,     6,    12,     0,   195,
       0,     0,     0,     0,     0,     0,     0,     0,     7,    13,
       8,     9,    10,     0,    20,     0,     0,    18,     0,    19,
       0,     0,   188,     0,     0,   175,   176,   182,   183,   184,
     185,   186,   188,     0,     0,   196,   197,   198,   199,   200,
     194,   193,     0,     0,     0,    23,   189,     0,     0,     0,
     178,   179,   201,    11,    21,     0,    17,     0,   181,   180,
       0,   190,    22
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -214,  -214,   179,  -214,   -77,  -214,   151,   476,   488,   165,
      18,   354,   233,   621,   630,   -12,  -213
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    68,    69,    70,   235,    71,    90,    91,    92,    93,
      94,    95,   105,    96,    97,   123,   228
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      98,   106,   112,    98,   237,    72,    98,    98,    98,    98,
      98,    98,   259,   261,   292,   245,   246,   247,   248,   249,
     245,   246,   247,   248,   249,   262,   263,   264,   265,   272,
     273,   104,   110,   116,   121,   125,   131,   139,   147,   155,
     163,   171,   178,   181,   247,   248,   249,   112,   112,   112,
     112,   112,   112,   300,   301,   266,   267,   218,   219,   268,
      73,   227,    74,   230,   274,   275,    75,   234,   236,   236,
     277,   278,    76,   240,   241,   242,   243,   194,   198,   202,
     206,   210,   214,   217,   279,   278,   221,   250,   251,     1,
       2,    77,     3,     4,     5,     6,     7,     8,   313,   278,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    78,   224,
      82,    83,   220,   254,   101,   107,    82,    83,    79,   127,
     135,   143,   151,   159,   167,   176,   179,    80,   103,    81,
     115,   223,    67,   130,   138,   146,   154,   162,   170,   266,
     267,   285,   286,   295,   229,   231,   302,   238,    82,    83,
     232,    82,    83,   244,   233,   239,   255,   256,   245,   246,
     247,   248,   249,   227,   260,   257,   258,   280,   269,    84,
     271,   281,   307,   286,    99,    84,   285,   294,   308,   225,
     226,   266,    87,   287,   288,   289,   290,   291,    87,   306,
     309,   310,    89,   312,   311,   318,   319,   222,    89,   122,
     296,   297,   298,   299,   227,   227,     0,    84,     0,     0,
      84,   236,    85,   303,   304,    99,   305,     0,    86,     0,
      87,    86,     0,    87,    88,     0,     0,   100,   282,   283,
      89,     0,     0,    89,   245,   246,   247,   248,   249,     0,
       0,     0,     0,     1,     2,   317,     3,     4,     5,     6,
       7,     8,     0,   320,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    82,    83,     0,    82,    83,     0,   111,   117,
       0,   126,   132,   140,   148,   156,   164,   172,     0,     0,
       0,    82,    83,     0,    82,    83,    67,     0,     0,     0,
       0,   314,   315,     0,     0,    82,    83,   245,   246,   247,
     248,   249,   270,     0,     0,     0,    82,    83,   245,   246,
     247,   248,   249,   195,   199,   203,   207,   211,   215,     0,
       0,    84,     0,     0,    84,     0,    99,     0,     0,    99,
       0,     0,    86,     0,    87,    86,     0,    87,    88,     0,
      84,   175,     0,    84,    89,    85,     0,    89,    99,     0,
       0,     0,     0,    87,    84,     0,    87,    88,     0,    99,
     100,     0,     0,    89,     0,    84,    89,    87,     0,     0,
      99,    88,     0,     0,     0,     0,     0,    89,    87,     0,
       0,     0,   175,     0,     0,     0,     0,     0,    89,   102,
     108,   113,   120,   124,   128,   136,   144,   152,   160,   168,
     177,   180,   109,   114,     0,     0,   129,   137,   145,   153,
     161,   169,   245,   246,   247,   248,   249,   262,   263,   264,
     265,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,     0,     0,     0,     0,   192,   196,   200,   204,   208,
     212,   216,     0,     0,     0,     0,     0,   193,   197,   201,
     205,   209,   213,     0,   292,     0,     0,     0,     0,   316,
     245,   246,   247,   248,   249,   245,   246,   247,   248,   249,
     321,     0,     0,     0,     0,   322,   245,   246,   247,   248,
     249,   245,   246,   247,   248,   249,   252,     0,     0,     0,
       0,   245,   246,   247,   248,   249,   253,     0,     0,     0,
       0,   245,   246,   247,   248,   249,   276,     0,     0,     0,
       0,   245,   246,   247,   248,   249,   284,     0,     0,     0,
       0,   245,   246,   247,   248,   249,   293,     0,     0,     0,
       0,   245,   246,   247,   248,   249,   118,     0,     0,   133,
     141,   149,   157,   165,   173,   119,     0,     0,   134,   142,
     150,   158,   166,   174
};

static const yytype_int16 yycheck[] =
{
      12,    13,    14,    15,    81,    76,    18,    19,    20,    21,
      22,    23,   225,   226,    77,    83,    84,    85,    86,    87,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    77,
      78,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    85,    86,    87,    59,    60,    61,
      62,    63,    64,   266,   267,    73,    74,    92,    93,    77,
      76,    73,    76,    75,    77,    78,    76,    79,    80,    81,
      77,    78,    76,    85,    86,    87,    88,    59,    60,    61,
      62,    63,    64,    65,    77,    78,     0,    99,   100,     3,
       4,    76,     6,     7,     8,     9,    10,    11,    77,    78,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    76,     5,
      12,    13,    79,   175,    13,    14,    12,    13,    76,    18,
      19,    20,    21,    22,    23,    24,    25,    76,    13,    76,
      15,    72,    96,    18,    19,    20,    21,    22,    23,    73,
      74,    80,    81,    77,    72,    72,   273,    76,    12,    13,
      72,    12,    13,    78,    72,    76,   218,   219,    83,    84,
      85,    86,    87,   225,   226,    77,    76,    72,    77,    71,
      77,    72,    78,    81,    76,    71,    80,    71,    78,    75,
      76,    73,    84,   245,   246,   247,   248,   249,    84,    77,
      80,    80,    94,    77,    81,    81,    77,    68,    94,    16,
     262,   263,   264,   265,   266,   267,    -1,    71,    -1,    -1,
      71,   273,    76,   275,   276,    76,   278,    -1,    82,    -1,
      84,    82,    -1,    84,    88,    -1,    -1,    88,    77,    78,
      94,    -1,    -1,    94,    83,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,     3,     4,   307,     6,     7,     8,     9,
      10,    11,    -1,   315,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    12,    13,    -1,    12,    13,    -1,    14,    15,
      -1,    17,    18,    19,    20,    21,    22,    23,    -1,    -1,
      -1,    12,    13,    -1,    12,    13,    96,    -1,    -1,    -1,
      -1,    77,    78,    -1,    -1,    12,    13,    83,    84,    85,
      86,    87,    77,    -1,    -1,    -1,    12,    13,    83,    84,
      85,    86,    87,    59,    60,    61,    62,    63,    64,    -1,
      -1,    71,    -1,    -1,    71,    -1,    76,    -1,    -1,    76,
      -1,    -1,    82,    -1,    84,    82,    -1,    84,    88,    -1,
      71,    88,    -1,    71,    94,    76,    -1,    94,    76,    -1,
      -1,    -1,    -1,    84,    71,    -1,    84,    88,    -1,    76,
      88,    -1,    -1,    94,    -1,    71,    94,    84,    -1,    -1,
      76,    88,    -1,    -1,    -1,    -1,    -1,    94,    84,    -1,
      -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    94,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    14,    15,    -1,    -1,    18,    19,    20,    21,
      22,    23,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    -1,    -1,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    59,    60,    61,
      62,    63,    64,    -1,    77,    -1,    -1,    -1,    -1,    77,
      83,    84,    85,    86,    87,    83,    84,    85,    86,    87,
      77,    -1,    -1,    -1,    -1,    77,    83,    84,    85,    86,
      87,    83,    84,    85,    86,    87,    78,    -1,    -1,    -1,
      -1,    83,    84,    85,    86,    87,    78,    -1,    -1,    -1,
      -1,    83,    84,    85,    86,    87,    78,    -1,    -1,    -1,
      -1,    83,    84,    85,    86,    87,    78,    -1,    -1,    -1,
      -1,    83,    84,    85,    86,    87,    78,    -1,    -1,    -1,
      -1,    83,    84,    85,    86,    87,    15,    -1,    -1,    18,
      19,    20,    21,    22,    23,    15,    -1,    -1,    18,    19,
      20,    21,    22,    23
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,     4,     6,     7,     8,     9,    10,    11,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    96,    98,    99,
     100,   102,    76,    76,    76,    76,    76,    76,    76,    76,
      76,    76,    12,    13,    71,    76,    82,    84,    88,    94,
     103,   104,   105,   106,   107,   108,   110,   111,   112,    76,
      88,   103,   104,   106,   107,   109,   112,   103,   104,   105,
     107,   108,   112,   104,   105,   106,   107,   108,   110,   111,
     104,   107,   109,   112,   104,   107,   108,   103,   104,   105,
     106,   107,   108,   110,   111,   103,   104,   105,   106,   107,
     108,   110,   111,   103,   104,   105,   106,   107,   108,   110,
     111,   103,   104,   105,   106,   107,   108,   110,   111,   103,
     104,   105,   106,   107,   108,   110,   111,   103,   104,   105,
     106,   107,   108,   110,   111,    88,   103,   104,   107,   103,
     104,   107,   104,   104,   104,   104,   104,   104,   104,   104,
     104,   104,   104,   105,   107,   108,   104,   105,   107,   108,
     104,   105,   107,   108,   104,   105,   107,   108,   104,   105,
     107,   108,   104,   105,   107,   108,   104,   107,    92,    93,
      79,     0,    99,    72,     5,    75,    76,   112,   113,    72,
     112,    72,    72,    72,   112,   101,   112,   101,    76,    76,
     112,   112,   112,   112,    78,    83,    84,    85,    86,    87,
     112,   112,    78,    78,   112,   112,   112,    77,    76,   113,
     112,   113,    88,    89,    90,    91,    73,    74,    77,    77,
      77,    77,    77,    78,    77,    78,    78,    77,    78,    77,
      72,    72,    77,    78,    78,    80,    81,   112,   112,   112,
     112,   112,    77,    78,    71,    77,   112,   112,   112,   112,
     113,   113,   101,   112,   112,   112,    77,    78,    78,    80,
      80,    81,    77,    77,    77,    78,    77,   112,    81,    77,
     112,    77,    77
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    97,    98,    98,    99,    99,    99,    99,    99,    99,
      99,    99,    99,    99,    99,    99,   100,   100,   100,   100,
     100,   100,   100,   101,   101,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   112,   112,   112,   112,   112,   112,   112,
     112,   112,   112,   113,   113,   113,   113,   113,   113,   113,
     113,   113
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     1,     2,     3,     3,     4,     4,     4,
       4,     6,     4,     4,     1,     1,     1,     6,     4,     4,
       4,     6,     8,     3,     1,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       1,     1,     1,     2,     2,     2,     2,     2,     2,     2,
       2,     1,     2,     2,     2,     2,     1,     2,     2,     2,
       2,     1,     2,     2,     2,     2,     1,     2,     2,     2,
       2,     2,     2,     2,     1,     3,     3,     2,     4,     4,
       5,     5,     3,     3,     3,     3,     3,     2,     3,     4,
       6,     1,     1,     3,     3,     2,     3,     3,     3,     3,
       3,     4
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 4: /* stmt: LABEL COLON  */
#line 186 "asm.y"
                    { new_label((yyvsp[-1].str)); }
#line 1484 "asm.tab.c"
    break;

  case 5: /* stmt: SYMBOL ASSIGN expr  */
#line 187 "asm.y"
                           { new_symbol_expr((yyvsp[-2].str), (yyvsp[0].expr)); }
#line 1490 "asm.tab.c"
    break;

  case 6: /* stmt: SYMBOL GUESS expr  */
#line 188 "asm.y"
                          { new_symbol_expr_guess((yyvsp[-2].str), (yyvsp[0].expr)); }
#line 1496 "asm.tab.c"
    break;

  case 7: /* stmt: IF LPAREN lexpr RPAREN  */
#line 189 "asm.y"
                               { push_if_state((yyvsp[-1].expr)); }
#line 1502 "asm.tab.c"
    break;

  case 8: /* stmt: ORG LPAREN expr RPAREN  */
#line 190 "asm.y"
                               { set_org((yyvsp[-1].expr)); }
#line 1508 "asm.tab.c"
    break;

  case 9: /* stmt: ERROR LPAREN STRING RPAREN  */
#line 191 "asm.y"
                                   { asm_error((yyvsp[-1].str)); }
#line 1514 "asm.tab.c"
    break;

  case 10: /* stmt: ECHO1 LPAREN STRING RPAREN  */
#line 192 "asm.y"
                                   { asm_echo((yyvsp[-1].str), NULL); }
#line 1520 "asm.tab.c"
    break;

  case 11: /* stmt: ECHO1 LPAREN STRING COMMA exprs RPAREN  */
#line 193 "asm.y"
                                               { asm_echo((yyvsp[-3].str), (yyvsp[-1].atom)); }
#line 1526 "asm.tab.c"
    break;

  case 12: /* stmt: INCLUDE LPAREN STRING RPAREN  */
#line 194 "asm.y"
                                     { asm_include((yyvsp[-1].str)); }
#line 1532 "asm.tab.c"
    break;

  case 13: /* stmt: MACRO LPAREN STRING RPAREN  */
#line 195 "asm.y"
                                   { push_macro_state((yyvsp[-1].str)); }
#line 1538 "asm.tab.c"
    break;

  case 14: /* stmt: atom  */
#line 196 "asm.y"
             { vec_push(asm_atoms, &(yyvsp[0].atom)); }
#line 1544 "asm.tab.c"
    break;

  case 15: /* stmt: MACRO_STRING  */
#line 197 "asm.y"
                     { macro_append((yyvsp[0].str)); }
#line 1550 "asm.tab.c"
    break;

  case 16: /* atom: op  */
#line 199 "asm.y"
           { (yyval.atom) = (yyvsp[0].atom); }
#line 1556 "asm.tab.c"
    break;

  case 17: /* atom: RES LPAREN expr COMMA expr RPAREN  */
#line 200 "asm.y"
                                          { (yyval.atom) = new_res((yyvsp[-3].expr), (yyvsp[-1].expr)); }
#line 1562 "asm.tab.c"
    break;

  case 18: /* atom: WORD LPAREN exprs RPAREN  */
#line 201 "asm.y"
                                 { (yyval.atom) = exprs_to_word_exprs((yyvsp[-1].atom)); }
#line 1568 "asm.tab.c"
    break;

  case 19: /* atom: BYTE LPAREN exprs RPAREN  */
#line 202 "asm.y"
                                 { (yyval.atom) = exprs_to_byte_exprs((yyvsp[-1].atom)); }
#line 1574 "asm.tab.c"
    break;

  case 20: /* atom: INCBIN LPAREN STRING RPAREN  */
#line 203 "asm.y"
                                    {
            (yyval.atom) = new_incbin((yyvsp[-1].str), NULL, NULL); }
#line 1581 "asm.tab.c"
    break;

  case 21: /* atom: INCBIN LPAREN STRING COMMA expr RPAREN  */
#line 205 "asm.y"
                                               {
            (yyval.atom) = new_incbin((yyvsp[-3].str), (yyvsp[-1].expr), NULL); }
#line 1588 "asm.tab.c"
    break;

  case 22: /* atom: INCBIN LPAREN STRING COMMA expr COMMA expr RPAREN  */
#line 207 "asm.y"
                                                          {
            (yyval.atom) = new_incbin((yyvsp[-5].str), (yyvsp[-3].expr), (yyvsp[-1].expr)); }
#line 1595 "asm.tab.c"
    break;

  case 23: /* exprs: exprs COMMA expr  */
#line 210 "asm.y"
                         { (yyval.atom) = exprs_add((yyvsp[-2].atom), (yyvsp[0].expr)); }
#line 1601 "asm.tab.c"
    break;

  case 24: /* exprs: expr  */
#line 211 "asm.y"
             { (yyval.atom) = new_exprs((yyvsp[0].expr)); }
#line 1607 "asm.tab.c"
    break;

  case 25: /* op: LDA am_im  */
#line 213 "asm.y"
                   { (yyval.atom) = new_op(0xA9, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 1613 "asm.tab.c"
    break;

  case 26: /* op: LDA am_zp  */
#line 214 "asm.y"
                   { (yyval.atom) = new_op(0xA5, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1619 "asm.tab.c"
    break;

  case 27: /* op: LDA am_zpx  */
#line 215 "asm.y"
                   { (yyval.atom) = new_op(0xB5, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1625 "asm.tab.c"
    break;

  case 28: /* op: LDA am_a  */
#line 216 "asm.y"
                   { (yyval.atom) = new_op(0xAD, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1631 "asm.tab.c"
    break;

  case 29: /* op: LDA am_ax  */
#line 217 "asm.y"
                   { (yyval.atom) = new_op(0xBD, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1637 "asm.tab.c"
    break;

  case 30: /* op: LDA am_ay  */
#line 218 "asm.y"
                   { (yyval.atom) = new_op(0xB9, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1643 "asm.tab.c"
    break;

  case 31: /* op: LDA am_ix  */
#line 219 "asm.y"
                   { (yyval.atom) = new_op(0xA1, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1649 "asm.tab.c"
    break;

  case 32: /* op: LDA am_iy  */
#line 220 "asm.y"
                   { (yyval.atom) = new_op(0xB1, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1655 "asm.tab.c"
    break;

  case 33: /* op: LDX am_im  */
#line 222 "asm.y"
                   { (yyval.atom) = new_op(0xA2, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 1661 "asm.tab.c"
    break;

  case 34: /* op: LDX am_zp  */
#line 223 "asm.y"
                   { (yyval.atom) = new_op(0xA6, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1667 "asm.tab.c"
    break;

  case 35: /* op: LDX am_zpy  */
#line 224 "asm.y"
                   { (yyval.atom) = new_op(0xB6, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1673 "asm.tab.c"
    break;

  case 36: /* op: LDX am_a  */
#line 225 "asm.y"
                   { (yyval.atom) = new_op(0xAE, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1679 "asm.tab.c"
    break;

  case 37: /* op: LDX am_ay  */
#line 226 "asm.y"
                   { (yyval.atom) = new_op(0xBE, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1685 "asm.tab.c"
    break;

  case 38: /* op: LDY am_im  */
#line 228 "asm.y"
                   { (yyval.atom) = new_op(0xA0, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 1691 "asm.tab.c"
    break;

  case 39: /* op: LDY am_zp  */
#line 229 "asm.y"
                   { (yyval.atom) = new_op(0xA4, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1697 "asm.tab.c"
    break;

  case 40: /* op: LDY am_zpx  */
#line 230 "asm.y"
                   { (yyval.atom) = new_op(0xB4, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1703 "asm.tab.c"
    break;

  case 41: /* op: LDY am_a  */
#line 231 "asm.y"
                   { (yyval.atom) = new_op(0xAC, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1709 "asm.tab.c"
    break;

  case 42: /* op: LDY am_ax  */
#line 232 "asm.y"
                   { (yyval.atom) = new_op(0xBC, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1715 "asm.tab.c"
    break;

  case 43: /* op: STA am_zp  */
#line 234 "asm.y"
                   { (yyval.atom) = new_op(0x85, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1721 "asm.tab.c"
    break;

  case 44: /* op: STA am_zpx  */
#line 235 "asm.y"
                   { (yyval.atom) = new_op(0x95, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1727 "asm.tab.c"
    break;

  case 45: /* op: STA am_a  */
#line 236 "asm.y"
                   { (yyval.atom) = new_op(0x8D, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1733 "asm.tab.c"
    break;

  case 46: /* op: STA am_ax  */
#line 237 "asm.y"
                   { (yyval.atom) = new_op(0x9D, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1739 "asm.tab.c"
    break;

  case 47: /* op: STA am_ay  */
#line 238 "asm.y"
                   { (yyval.atom) = new_op(0x99, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1745 "asm.tab.c"
    break;

  case 48: /* op: STA am_ix  */
#line 239 "asm.y"
                   { (yyval.atom) = new_op(0x81, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1751 "asm.tab.c"
    break;

  case 49: /* op: STA am_iy  */
#line 240 "asm.y"
                   { (yyval.atom) = new_op(0x91, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1757 "asm.tab.c"
    break;

  case 50: /* op: STX am_zp  */
#line 242 "asm.y"
                   { (yyval.atom) = new_op(0x86, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1763 "asm.tab.c"
    break;

  case 51: /* op: STX am_zpy  */
#line 243 "asm.y"
                   { (yyval.atom) = new_op(0x96, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1769 "asm.tab.c"
    break;

  case 52: /* op: STX am_a  */
#line 244 "asm.y"
                   { (yyval.atom) = new_op(0x8e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1775 "asm.tab.c"
    break;

  case 53: /* op: STY am_zp  */
#line 246 "asm.y"
                   { (yyval.atom) = new_op(0x84, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1781 "asm.tab.c"
    break;

  case 54: /* op: STY am_zpx  */
#line 247 "asm.y"
                   { (yyval.atom) = new_op(0x94, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1787 "asm.tab.c"
    break;

  case 55: /* op: STY am_a  */
#line 248 "asm.y"
                   { (yyval.atom) = new_op(0x8c, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1793 "asm.tab.c"
    break;

  case 56: /* op: AND am_im  */
#line 250 "asm.y"
                   { (yyval.atom) = new_op(0x29, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 1799 "asm.tab.c"
    break;

  case 57: /* op: AND am_zp  */
#line 251 "asm.y"
                   { (yyval.atom) = new_op(0x25, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1805 "asm.tab.c"
    break;

  case 58: /* op: AND am_zpx  */
#line 252 "asm.y"
                   { (yyval.atom) = new_op(0x35, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1811 "asm.tab.c"
    break;

  case 59: /* op: AND am_a  */
#line 253 "asm.y"
                   { (yyval.atom) = new_op(0x2d, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1817 "asm.tab.c"
    break;

  case 60: /* op: AND am_ax  */
#line 254 "asm.y"
                   { (yyval.atom) = new_op(0x3d, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1823 "asm.tab.c"
    break;

  case 61: /* op: AND am_ay  */
#line 255 "asm.y"
                   { (yyval.atom) = new_op(0x39, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1829 "asm.tab.c"
    break;

  case 62: /* op: AND am_ix  */
#line 256 "asm.y"
                   { (yyval.atom) = new_op(0x21, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1835 "asm.tab.c"
    break;

  case 63: /* op: AND am_iy  */
#line 257 "asm.y"
                   { (yyval.atom) = new_op(0x31, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1841 "asm.tab.c"
    break;

  case 64: /* op: ORA am_im  */
#line 259 "asm.y"
                   { (yyval.atom) = new_op(0x09, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 1847 "asm.tab.c"
    break;

  case 65: /* op: ORA am_zp  */
#line 260 "asm.y"
                   { (yyval.atom) = new_op(0x05, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1853 "asm.tab.c"
    break;

  case 66: /* op: ORA am_zpx  */
#line 261 "asm.y"
                   { (yyval.atom) = new_op(0x15, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1859 "asm.tab.c"
    break;

  case 67: /* op: ORA am_a  */
#line 262 "asm.y"
                   { (yyval.atom) = new_op(0x0d, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1865 "asm.tab.c"
    break;

  case 68: /* op: ORA am_ax  */
#line 263 "asm.y"
                   { (yyval.atom) = new_op(0x1d, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1871 "asm.tab.c"
    break;

  case 69: /* op: ORA am_ay  */
#line 264 "asm.y"
                   { (yyval.atom) = new_op(0x19, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1877 "asm.tab.c"
    break;

  case 70: /* op: ORA am_ix  */
#line 265 "asm.y"
                   { (yyval.atom) = new_op(0x01, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1883 "asm.tab.c"
    break;

  case 71: /* op: ORA am_iy  */
#line 266 "asm.y"
                   { (yyval.atom) = new_op(0x11, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1889 "asm.tab.c"
    break;

  case 72: /* op: EOR am_im  */
#line 268 "asm.y"
                   { (yyval.atom) = new_op(0x49, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 1895 "asm.tab.c"
    break;

  case 73: /* op: EOR am_zp  */
#line 269 "asm.y"
                   { (yyval.atom) = new_op(0x45, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1901 "asm.tab.c"
    break;

  case 74: /* op: EOR am_zpx  */
#line 270 "asm.y"
                   { (yyval.atom) = new_op(0x55, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1907 "asm.tab.c"
    break;

  case 75: /* op: EOR am_a  */
#line 271 "asm.y"
                   { (yyval.atom) = new_op(0x4d, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1913 "asm.tab.c"
    break;

  case 76: /* op: EOR am_ax  */
#line 272 "asm.y"
                   { (yyval.atom) = new_op(0x5d, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1919 "asm.tab.c"
    break;

  case 77: /* op: EOR am_ay  */
#line 273 "asm.y"
                   { (yyval.atom) = new_op(0x59, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1925 "asm.tab.c"
    break;

  case 78: /* op: EOR am_ix  */
#line 274 "asm.y"
                   { (yyval.atom) = new_op(0x41, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1931 "asm.tab.c"
    break;

  case 79: /* op: EOR am_iy  */
#line 275 "asm.y"
                   { (yyval.atom) = new_op(0x51, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1937 "asm.tab.c"
    break;

  case 80: /* op: ADC am_im  */
#line 277 "asm.y"
                   { (yyval.atom) = new_op(0x69, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 1943 "asm.tab.c"
    break;

  case 81: /* op: ADC am_zp  */
#line 278 "asm.y"
                   { (yyval.atom) = new_op(0x65, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1949 "asm.tab.c"
    break;

  case 82: /* op: ADC am_zpx  */
#line 279 "asm.y"
                   { (yyval.atom) = new_op(0x75, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1955 "asm.tab.c"
    break;

  case 83: /* op: ADC am_a  */
#line 280 "asm.y"
                   { (yyval.atom) = new_op(0x6D, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1961 "asm.tab.c"
    break;

  case 84: /* op: ADC am_ax  */
#line 281 "asm.y"
                   { (yyval.atom) = new_op(0x7D, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1967 "asm.tab.c"
    break;

  case 85: /* op: ADC am_ay  */
#line 282 "asm.y"
                   { (yyval.atom) = new_op(0x79, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 1973 "asm.tab.c"
    break;

  case 86: /* op: ADC am_ix  */
#line 283 "asm.y"
                   { (yyval.atom) = new_op(0x61, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1979 "asm.tab.c"
    break;

  case 87: /* op: ADC am_iy  */
#line 284 "asm.y"
                   { (yyval.atom) = new_op(0x71, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 1985 "asm.tab.c"
    break;

  case 88: /* op: SBC am_im  */
#line 286 "asm.y"
                   { (yyval.atom) = new_op(0xe9, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 1991 "asm.tab.c"
    break;

  case 89: /* op: SBC am_zp  */
#line 287 "asm.y"
                   { (yyval.atom) = new_op(0xe5, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 1997 "asm.tab.c"
    break;

  case 90: /* op: SBC am_zpx  */
#line 288 "asm.y"
                   { (yyval.atom) = new_op(0xf5, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2003 "asm.tab.c"
    break;

  case 91: /* op: SBC am_a  */
#line 289 "asm.y"
                   { (yyval.atom) = new_op(0xeD, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2009 "asm.tab.c"
    break;

  case 92: /* op: SBC am_ax  */
#line 290 "asm.y"
                   { (yyval.atom) = new_op(0xfD, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2015 "asm.tab.c"
    break;

  case 93: /* op: SBC am_ay  */
#line 291 "asm.y"
                   { (yyval.atom) = new_op(0xf9, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2021 "asm.tab.c"
    break;

  case 94: /* op: SBC am_ix  */
#line 292 "asm.y"
                   { (yyval.atom) = new_op(0xe1, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2027 "asm.tab.c"
    break;

  case 95: /* op: SBC am_iy  */
#line 293 "asm.y"
                   { (yyval.atom) = new_op(0xf1, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2033 "asm.tab.c"
    break;

  case 96: /* op: CMP am_im  */
#line 295 "asm.y"
                   { (yyval.atom) = new_op(0xc9, ATOM_TYPE_OP_ARG_UI8, (yyvsp[0].expr)); }
#line 2039 "asm.tab.c"
    break;

  case 97: /* op: CMP am_zp  */
#line 296 "asm.y"
                   { (yyval.atom) = new_op(0xc5, ATOM_TYPE_OP_ARG_U8,  (yyvsp[0].expr)); }
#line 2045 "asm.tab.c"
    break;

  case 98: /* op: CMP am_zpx  */
#line 297 "asm.y"
                   { (yyval.atom) = new_op(0xd5, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2051 "asm.tab.c"
    break;

  case 99: /* op: CMP am_a  */
#line 298 "asm.y"
                   { (yyval.atom) = new_op(0xcD, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2057 "asm.tab.c"
    break;

  case 100: /* op: CMP am_ax  */
#line 299 "asm.y"
                   { (yyval.atom) = new_op(0xdD, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2063 "asm.tab.c"
    break;

  case 101: /* op: CMP am_ay  */
#line 300 "asm.y"
                   { (yyval.atom) = new_op(0xd9, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2069 "asm.tab.c"
    break;

  case 102: /* op: CMP am_ix  */
#line 301 "asm.y"
                   { (yyval.atom) = new_op(0xc1, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2075 "asm.tab.c"
    break;

  case 103: /* op: CMP am_iy  */
#line 302 "asm.y"
                   { (yyval.atom) = new_op(0xd1, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2081 "asm.tab.c"
    break;

  case 104: /* op: CPX am_im  */
#line 304 "asm.y"
                  { (yyval.atom) = new_op(0xe0, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2087 "asm.tab.c"
    break;

  case 105: /* op: CPX am_zp  */
#line 305 "asm.y"
                  { (yyval.atom) = new_op(0xe4, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2093 "asm.tab.c"
    break;

  case 106: /* op: CPX am_a  */
#line 306 "asm.y"
                  { (yyval.atom) = new_op(0xec, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2099 "asm.tab.c"
    break;

  case 107: /* op: CPY am_im  */
#line 307 "asm.y"
                  { (yyval.atom) = new_op(0xc0, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2105 "asm.tab.c"
    break;

  case 108: /* op: CPY am_zp  */
#line 308 "asm.y"
                  { (yyval.atom) = new_op(0xc4, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2111 "asm.tab.c"
    break;

  case 109: /* op: CPY am_a  */
#line 309 "asm.y"
                  { (yyval.atom) = new_op(0xcc, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2117 "asm.tab.c"
    break;

  case 110: /* op: TXS  */
#line 311 "asm.y"
            { (yyval.atom) = new_op0(0x9A); }
#line 2123 "asm.tab.c"
    break;

  case 111: /* op: TSX  */
#line 312 "asm.y"
            { (yyval.atom) = new_op0(0xBA); }
#line 2129 "asm.tab.c"
    break;

  case 112: /* op: PHA  */
#line 313 "asm.y"
            { (yyval.atom) = new_op0(0x48); }
#line 2135 "asm.tab.c"
    break;

  case 113: /* op: PLA  */
#line 314 "asm.y"
            { (yyval.atom) = new_op0(0x68); }
#line 2141 "asm.tab.c"
    break;

  case 114: /* op: PHP  */
#line 315 "asm.y"
            { (yyval.atom) = new_op0(0x08); }
#line 2147 "asm.tab.c"
    break;

  case 115: /* op: PLP  */
#line 316 "asm.y"
            { (yyval.atom) = new_op0(0x28); }
#line 2153 "asm.tab.c"
    break;

  case 116: /* op: SEI  */
#line 317 "asm.y"
            { (yyval.atom) = new_op0(0x78); }
#line 2159 "asm.tab.c"
    break;

  case 117: /* op: CLI  */
#line 318 "asm.y"
            { (yyval.atom) = new_op0(0x58); }
#line 2165 "asm.tab.c"
    break;

  case 118: /* op: NOP  */
#line 319 "asm.y"
            { (yyval.atom) = new_op0(0xea); }
#line 2171 "asm.tab.c"
    break;

  case 119: /* op: TYA  */
#line 320 "asm.y"
            { (yyval.atom) = new_op0(0x98); }
#line 2177 "asm.tab.c"
    break;

  case 120: /* op: TAY  */
#line 321 "asm.y"
            { (yyval.atom) = new_op0(0xa8); }
#line 2183 "asm.tab.c"
    break;

  case 121: /* op: TXA  */
#line 322 "asm.y"
            { (yyval.atom) = new_op0(0x8a); }
#line 2189 "asm.tab.c"
    break;

  case 122: /* op: TAX  */
#line 323 "asm.y"
            { (yyval.atom) = new_op0(0xaa); }
#line 2195 "asm.tab.c"
    break;

  case 123: /* op: CLC  */
#line 324 "asm.y"
            { (yyval.atom) = new_op0(0x18); }
#line 2201 "asm.tab.c"
    break;

  case 124: /* op: SEC  */
#line 325 "asm.y"
            { (yyval.atom) = new_op0(0x38); }
#line 2207 "asm.tab.c"
    break;

  case 125: /* op: RTS  */
#line 326 "asm.y"
            { (yyval.atom) = new_op0(0x60); }
#line 2213 "asm.tab.c"
    break;

  case 126: /* op: CLV  */
#line 327 "asm.y"
            { (yyval.atom) = new_op0(0xb8); }
#line 2219 "asm.tab.c"
    break;

  case 127: /* op: CLD  */
#line 328 "asm.y"
            { (yyval.atom) = new_op0(0xd8); }
#line 2225 "asm.tab.c"
    break;

  case 128: /* op: SED  */
#line 329 "asm.y"
            { (yyval.atom) = new_op0(0xf0); }
#line 2231 "asm.tab.c"
    break;

  case 129: /* op: JSR am_a  */
#line 331 "asm.y"
                   { (yyval.atom) = new_op(0x20, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2237 "asm.tab.c"
    break;

  case 130: /* op: JMP am_a  */
#line 332 "asm.y"
                   { (yyval.atom) = new_op(0x4c, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2243 "asm.tab.c"
    break;

  case 131: /* op: BEQ am_a  */
#line 333 "asm.y"
                   { (yyval.atom) = new_op(0xf0, ATOM_TYPE_OP_ARG_I8,  (yyvsp[0].expr)); }
#line 2249 "asm.tab.c"
    break;

  case 132: /* op: BNE am_a  */
#line 334 "asm.y"
                   { (yyval.atom) = new_op(0xd0, ATOM_TYPE_OP_ARG_I8,  (yyvsp[0].expr)); }
#line 2255 "asm.tab.c"
    break;

  case 133: /* op: BCC am_a  */
#line 335 "asm.y"
                   { (yyval.atom) = new_op(0x90, ATOM_TYPE_OP_ARG_I8,  (yyvsp[0].expr)); }
#line 2261 "asm.tab.c"
    break;

  case 134: /* op: BCS am_a  */
#line 336 "asm.y"
                   { (yyval.atom) = new_op(0xb0, ATOM_TYPE_OP_ARG_I8,  (yyvsp[0].expr)); }
#line 2267 "asm.tab.c"
    break;

  case 135: /* op: BPL am_a  */
#line 337 "asm.y"
                   { (yyval.atom) = new_op(0x10, ATOM_TYPE_OP_ARG_I8,  (yyvsp[0].expr)); }
#line 2273 "asm.tab.c"
    break;

  case 136: /* op: BMI am_a  */
#line 338 "asm.y"
                   { (yyval.atom) = new_op(0x30, ATOM_TYPE_OP_ARG_I8,  (yyvsp[0].expr)); }
#line 2279 "asm.tab.c"
    break;

  case 137: /* op: BVC am_a  */
#line 339 "asm.y"
                   { (yyval.atom) = new_op(0x50, ATOM_TYPE_OP_ARG_I8,  (yyvsp[0].expr)); }
#line 2285 "asm.tab.c"
    break;

  case 138: /* op: BVS am_a  */
#line 340 "asm.y"
                   { (yyval.atom) = new_op(0x70, ATOM_TYPE_OP_ARG_I8,  (yyvsp[0].expr)); }
#line 2291 "asm.tab.c"
    break;

  case 139: /* op: INX  */
#line 342 "asm.y"
            { (yyval.atom) = new_op0(0xe8); }
#line 2297 "asm.tab.c"
    break;

  case 140: /* op: DEX  */
#line 343 "asm.y"
            { (yyval.atom) = new_op0(0xca); }
#line 2303 "asm.tab.c"
    break;

  case 141: /* op: INY  */
#line 344 "asm.y"
            { (yyval.atom) = new_op0(0xc8); }
#line 2309 "asm.tab.c"
    break;

  case 142: /* op: DEY  */
#line 345 "asm.y"
            { (yyval.atom) = new_op0(0x88); }
#line 2315 "asm.tab.c"
    break;

  case 143: /* op: INC am_zp  */
#line 347 "asm.y"
                   { (yyval.atom) = new_op(0xe6, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2321 "asm.tab.c"
    break;

  case 144: /* op: INC am_zpx  */
#line 348 "asm.y"
                   { (yyval.atom) = new_op(0xf6, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2327 "asm.tab.c"
    break;

  case 145: /* op: INC am_a  */
#line 349 "asm.y"
                   { (yyval.atom) = new_op(0xee, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2333 "asm.tab.c"
    break;

  case 146: /* op: INC am_ax  */
#line 350 "asm.y"
                   { (yyval.atom) = new_op(0xfe, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2339 "asm.tab.c"
    break;

  case 147: /* op: DEC am_zp  */
#line 352 "asm.y"
                   { (yyval.atom) = new_op(0xc6, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2345 "asm.tab.c"
    break;

  case 148: /* op: DEC am_zpx  */
#line 353 "asm.y"
                   { (yyval.atom) = new_op(0xd6, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2351 "asm.tab.c"
    break;

  case 149: /* op: DEC am_a  */
#line 354 "asm.y"
                   { (yyval.atom) = new_op(0xce, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2357 "asm.tab.c"
    break;

  case 150: /* op: DEC am_ax  */
#line 355 "asm.y"
                   { (yyval.atom) = new_op(0xde, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2363 "asm.tab.c"
    break;

  case 151: /* op: LSR  */
#line 357 "asm.y"
                   { (yyval.atom) = new_op0(0x4a); }
#line 2369 "asm.tab.c"
    break;

  case 152: /* op: LSR am_zp  */
#line 358 "asm.y"
                   { (yyval.atom) = new_op(0x46, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2375 "asm.tab.c"
    break;

  case 153: /* op: LSR am_zpx  */
#line 359 "asm.y"
                   { (yyval.atom) = new_op(0x56, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2381 "asm.tab.c"
    break;

  case 154: /* op: LSR am_a  */
#line 360 "asm.y"
                   { (yyval.atom) = new_op(0x4e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2387 "asm.tab.c"
    break;

  case 155: /* op: LSR am_ax  */
#line 361 "asm.y"
                   { (yyval.atom) = new_op(0x5e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2393 "asm.tab.c"
    break;

  case 156: /* op: ASL  */
#line 363 "asm.y"
                   { (yyval.atom) = new_op0(0x0a); }
#line 2399 "asm.tab.c"
    break;

  case 157: /* op: ASL am_zp  */
#line 364 "asm.y"
                   { (yyval.atom) = new_op(0x06, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2405 "asm.tab.c"
    break;

  case 158: /* op: ASL am_zpx  */
#line 365 "asm.y"
                   { (yyval.atom) = new_op(0x16, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2411 "asm.tab.c"
    break;

  case 159: /* op: ASL am_a  */
#line 366 "asm.y"
                   { (yyval.atom) = new_op(0x0e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2417 "asm.tab.c"
    break;

  case 160: /* op: ASL am_ax  */
#line 367 "asm.y"
                   { (yyval.atom) = new_op(0x1e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2423 "asm.tab.c"
    break;

  case 161: /* op: ROR  */
#line 369 "asm.y"
                   { (yyval.atom) = new_op0(0x6a); }
#line 2429 "asm.tab.c"
    break;

  case 162: /* op: ROR am_zp  */
#line 370 "asm.y"
                   { (yyval.atom) = new_op(0x66, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2435 "asm.tab.c"
    break;

  case 163: /* op: ROR am_zpx  */
#line 371 "asm.y"
                   { (yyval.atom) = new_op(0x76, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2441 "asm.tab.c"
    break;

  case 164: /* op: ROR am_a  */
#line 372 "asm.y"
                   { (yyval.atom) = new_op(0x6e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2447 "asm.tab.c"
    break;

  case 165: /* op: ROR am_ax  */
#line 373 "asm.y"
                   { (yyval.atom) = new_op(0x7e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2453 "asm.tab.c"
    break;

  case 166: /* op: ROL  */
#line 375 "asm.y"
                   { (yyval.atom) = new_op0(0x2a); }
#line 2459 "asm.tab.c"
    break;

  case 167: /* op: ROL am_zp  */
#line 376 "asm.y"
                   { (yyval.atom) = new_op(0x26, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2465 "asm.tab.c"
    break;

  case 168: /* op: ROL am_zpx  */
#line 377 "asm.y"
                   { (yyval.atom) = new_op(0x36, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2471 "asm.tab.c"
    break;

  case 169: /* op: ROL am_a  */
#line 378 "asm.y"
                   { (yyval.atom) = new_op(0x2e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2477 "asm.tab.c"
    break;

  case 170: /* op: ROL am_ax  */
#line 379 "asm.y"
                   { (yyval.atom) = new_op(0x3e, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2483 "asm.tab.c"
    break;

  case 171: /* op: BIT am_zp  */
#line 381 "asm.y"
                   { (yyval.atom) = new_op(0x24, ATOM_TYPE_OP_ARG_U8, (yyvsp[0].expr)); }
#line 2489 "asm.tab.c"
    break;

  case 172: /* op: BIT am_a  */
#line 382 "asm.y"
                   { (yyval.atom) = new_op(0x2c, ATOM_TYPE_OP_ARG_U16, (yyvsp[0].expr)); }
#line 2495 "asm.tab.c"
    break;

  case 173: /* am_im: HASH expr  */
#line 384 "asm.y"
                  { (yyval.expr) = (yyvsp[0].expr); }
#line 2501 "asm.tab.c"
    break;

  case 174: /* am_a: expr  */
#line 385 "asm.y"
             { (yyval.expr) = (yyvsp[0].expr); }
#line 2507 "asm.tab.c"
    break;

  case 175: /* am_ax: expr COMMA X  */
#line 386 "asm.y"
                     { (yyval.expr) = (yyvsp[-2].expr); }
#line 2513 "asm.tab.c"
    break;

  case 176: /* am_ay: expr COMMA Y  */
#line 387 "asm.y"
                     { (yyval.expr) = (yyvsp[-2].expr); }
#line 2519 "asm.tab.c"
    break;

  case 177: /* am_zp: LT expr  */
#line 388 "asm.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2525 "asm.tab.c"
    break;

  case 178: /* am_zpx: LT expr COMMA X  */
#line 389 "asm.y"
                        { (yyval.expr) = (yyvsp[-2].expr); }
#line 2531 "asm.tab.c"
    break;

  case 179: /* am_zpy: LT expr COMMA Y  */
#line 390 "asm.y"
                        { (yyval.expr) = (yyvsp[-2].expr); }
#line 2537 "asm.tab.c"
    break;

  case 180: /* am_ix: LPAREN expr COMMA X RPAREN  */
#line 391 "asm.y"
                                   { (yyval.expr) = (yyvsp[-3].expr); }
#line 2543 "asm.tab.c"
    break;

  case 181: /* am_iy: LPAREN expr RPAREN COMMA Y  */
#line 392 "asm.y"
                                   { (yyval.expr) = (yyvsp[-3].expr); }
#line 2549 "asm.tab.c"
    break;

  case 182: /* expr: expr PLUS expr  */
#line 394 "asm.y"
                              { (yyval.expr) = new_expr_op2(PLUS, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2555 "asm.tab.c"
    break;

  case 183: /* expr: expr MINUS expr  */
#line 395 "asm.y"
                              { (yyval.expr) = new_expr_op2(MINUS, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2561 "asm.tab.c"
    break;

  case 184: /* expr: expr MULT expr  */
#line 396 "asm.y"
                              { (yyval.expr) = new_expr_op2(MULT, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2567 "asm.tab.c"
    break;

  case 185: /* expr: expr DIV expr  */
#line 397 "asm.y"
                              { (yyval.expr) = new_expr_op2(DIV, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2573 "asm.tab.c"
    break;

  case 186: /* expr: expr MOD expr  */
#line 398 "asm.y"
                              { (yyval.expr) = new_expr_op2(MOD, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2579 "asm.tab.c"
    break;

  case 187: /* expr: MINUS expr  */
#line 399 "asm.y"
                              { (yyval.expr) = new_expr_op1(vNEG, (yyvsp[0].expr)); }
#line 2585 "asm.tab.c"
    break;

  case 188: /* expr: LPAREN expr RPAREN  */
#line 400 "asm.y"
                           { (yyval.expr) = (yyvsp[-1].expr); }
#line 2591 "asm.tab.c"
    break;

  case 189: /* expr: INCLEN LPAREN STRING RPAREN  */
#line 401 "asm.y"
                                    { (yyval.expr) = new_expr_inclen((yyvsp[-1].str)); }
#line 2597 "asm.tab.c"
    break;

  case 190: /* expr: INCWORD LPAREN STRING COMMA expr RPAREN  */
#line 402 "asm.y"
                                                {
            (yyval.expr) = new_expr_incword((yyvsp[-3].str), (yyvsp[-1].expr)); }
#line 2604 "asm.tab.c"
    break;

  case 191: /* expr: NUMBER  */
#line 404 "asm.y"
               { (yyval.expr) = new_expr_number((yyvsp[0].num)); }
#line 2610 "asm.tab.c"
    break;

  case 192: /* expr: SYMBOL  */
#line 405 "asm.y"
               { (yyval.expr) = new_expr_symref((yyvsp[0].str)); }
#line 2616 "asm.tab.c"
    break;

  case 193: /* lexpr: lexpr LOR lexpr  */
#line 407 "asm.y"
                            { (yyval.expr) = new_expr_op2(LOR, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2622 "asm.tab.c"
    break;

  case 194: /* lexpr: lexpr LAND lexpr  */
#line 408 "asm.y"
                            { (yyval.expr) = new_expr_op2(LAND, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2628 "asm.tab.c"
    break;

  case 195: /* lexpr: LNOT lexpr  */
#line 409 "asm.y"
                            { (yyval.expr) = new_expr_op1(LNOT, (yyvsp[0].expr)); }
#line 2634 "asm.tab.c"
    break;

  case 196: /* lexpr: LPAREN lexpr RPAREN  */
#line 410 "asm.y"
                            { (yyval.expr) = (yyvsp[-1].expr); }
#line 2640 "asm.tab.c"
    break;

  case 197: /* lexpr: expr LT expr  */
#line 411 "asm.y"
                            { (yyval.expr) = new_expr_op2(LT, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2646 "asm.tab.c"
    break;

  case 198: /* lexpr: expr GT expr  */
#line 412 "asm.y"
                            { (yyval.expr) = new_expr_op2(GT, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2652 "asm.tab.c"
    break;

  case 199: /* lexpr: expr EQ expr  */
#line 413 "asm.y"
                            { (yyval.expr) = new_expr_op2(EQ, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2658 "asm.tab.c"
    break;

  case 200: /* lexpr: expr NEQ expr  */
#line 414 "asm.y"
                            { (yyval.expr) = new_expr_op2(NEQ, (yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2664 "asm.tab.c"
    break;

  case 201: /* lexpr: DEFINED LPAREN SYMBOL RPAREN  */
#line 416 "asm.y"
                                            { (yyval.expr) = new_is_defined((yyvsp[-1].str)); }
#line 2670 "asm.tab.c"
    break;


#line 2674 "asm.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 418 "asm.y"


void yyerror (const char *s)
{
    fprintf (stderr, "line %d, %s\n", num_lines, s);
}

void asm_set_source(struct membuf *buffer);

int assembleSinglePass(struct membuf *source, struct membuf *dest)
{
    int val;

    yydebug = 0;
    asm_src_buffer_push(source);
    vec_init(asm_atoms, sizeof(struct atom*));
    val = yyparse();
    if(val == 0)
    {
        output_atoms(dest, asm_atoms);
    }
    vec_free(asm_atoms, NULL);
    return val;
}
