/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_ASM_TAB_H_INCLUDED
# define YY_YY_ASM_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    INCLUDE = 258,                 /* INCLUDE  */
    IF = 259,                      /* IF  */
    DEFINED = 260,                 /* DEFINED  */
    MACRO = 261,                   /* MACRO  */
    MACRO_STRING = 262,            /* MACRO_STRING  */
    ORG = 263,                     /* ORG  */
    ERROR = 264,                   /* ERROR  */
    ECHO1 = 265,                   /* ECHO1  */
    INCBIN = 266,                  /* INCBIN  */
    INCLEN = 267,                  /* INCLEN  */
    INCWORD = 268,                 /* INCWORD  */
    RES = 269,                     /* RES  */
    WORD = 270,                    /* WORD  */
    BYTE = 271,                    /* BYTE  */
    LDA = 272,                     /* LDA  */
    LDX = 273,                     /* LDX  */
    LDY = 274,                     /* LDY  */
    STA = 275,                     /* STA  */
    STX = 276,                     /* STX  */
    STY = 277,                     /* STY  */
    AND = 278,                     /* AND  */
    ORA = 279,                     /* ORA  */
    EOR = 280,                     /* EOR  */
    ADC = 281,                     /* ADC  */
    SBC = 282,                     /* SBC  */
    CMP = 283,                     /* CMP  */
    CPX = 284,                     /* CPX  */
    CPY = 285,                     /* CPY  */
    TSX = 286,                     /* TSX  */
    TXS = 287,                     /* TXS  */
    PHA = 288,                     /* PHA  */
    PLA = 289,                     /* PLA  */
    PHP = 290,                     /* PHP  */
    PLP = 291,                     /* PLP  */
    SEI = 292,                     /* SEI  */
    CLI = 293,                     /* CLI  */
    NOP = 294,                     /* NOP  */
    TYA = 295,                     /* TYA  */
    TAY = 296,                     /* TAY  */
    TXA = 297,                     /* TXA  */
    TAX = 298,                     /* TAX  */
    CLC = 299,                     /* CLC  */
    SEC = 300,                     /* SEC  */
    RTS = 301,                     /* RTS  */
    CLV = 302,                     /* CLV  */
    CLD = 303,                     /* CLD  */
    SED = 304,                     /* SED  */
    JSR = 305,                     /* JSR  */
    JMP = 306,                     /* JMP  */
    BEQ = 307,                     /* BEQ  */
    BNE = 308,                     /* BNE  */
    BCC = 309,                     /* BCC  */
    BCS = 310,                     /* BCS  */
    BPL = 311,                     /* BPL  */
    BMI = 312,                     /* BMI  */
    BVC = 313,                     /* BVC  */
    BVS = 314,                     /* BVS  */
    INX = 315,                     /* INX  */
    DEX = 316,                     /* DEX  */
    INY = 317,                     /* INY  */
    DEY = 318,                     /* DEY  */
    INC = 319,                     /* INC  */
    DEC = 320,                     /* DEC  */
    LSR = 321,                     /* LSR  */
    ASL = 322,                     /* ASL  */
    ROR = 323,                     /* ROR  */
    ROL = 324,                     /* ROL  */
    BIT = 325,                     /* BIT  */
    SYMBOL = 326,                  /* SYMBOL  */
    STRING = 327,                  /* STRING  */
    LAND = 328,                    /* LAND  */
    LOR = 329,                     /* LOR  */
    LNOT = 330,                    /* LNOT  */
    LPAREN = 331,                  /* LPAREN  */
    RPAREN = 332,                  /* RPAREN  */
    COMMA = 333,                   /* COMMA  */
    COLON = 334,                   /* COLON  */
    X = 335,                       /* X  */
    Y = 336,                       /* Y  */
    HASH = 337,                    /* HASH  */
    PLUS = 338,                    /* PLUS  */
    MINUS = 339,                   /* MINUS  */
    MULT = 340,                    /* MULT  */
    DIV = 341,                     /* DIV  */
    MOD = 342,                     /* MOD  */
    LT = 343,                      /* LT  */
    GT = 344,                      /* GT  */
    EQ = 345,                      /* EQ  */
    NEQ = 346,                     /* NEQ  */
    ASSIGN = 347,                  /* ASSIGN  */
    GUESS = 348,                   /* GUESS  */
    NUMBER = 349,                  /* NUMBER  */
    vNEG = 350,                    /* vNEG  */
    LABEL = 351                    /* LABEL  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 148 "asm.y"

    i32 num;
    char *str;
    struct atom *atom;
    struct expr *expr;

#line 167 "asm.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_ASM_TAB_H_INCLUDED  */
