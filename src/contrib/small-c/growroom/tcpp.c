/* tcpp.c - Small-C Preprocessor
 * -----------------------------
 */

/* 25-Jan-1989  v0.70 - A.J.Travis
 * 28-Jul-1991  v0.71 - J.G.Harston
 *   system-specifics removed to local.h
 *   filenames use '/' instead of '_'
 *   main() ends with exit(0);, linesize incresed from 81 to 128
 * v0.73 - __BBC__ defined
 */

#include <stdio.h>
#include <ctype.h>

/*
 * boolean constants
 */
#define TRUE		1
#define FALSE		0
#define ERROR	       -1
#define UNDEFINED    NULL
#define SAME		0

/*
 * symbol table parameters
 */
#define HASHSIZE     257
#define DEFSIZE     4000
#define SYMSIZE     4000
#define PARMSIZE     132
#define NEXTPTR        0
#define BODYPTR        2
#define NAME           4
#define MAXPARMS      32
#define LINESIZE     128		/* max length of line + '\0' */

/*
 * directory containing 'standard' #include files
 */
#ifdef MSDOS
#define INCLUDE	"/include/"
#else
#include "local.h"
#endif

/*
 * function types
 */
char *filename();
char *hashfind();

/*
 * global variables
 */
char ibuf[BUFSIZ];
char *ip;
char obuf[BUFSIZ];
char *op;
char mac[BUFSIZ];
char name[LINESIZE];
char sbuf[LINESIZE];
char deftab[DEFSIZE];
char *defptr;
char *maxdef;
char symtab[SYMSIZE];
char *freeptr;
char *maxsym;
char infile[LINESIZE];
char outfile[LINESIZE];
FILE *file[4];
int filen;
int eof;
FILE *in;
FILE *out;
int hashtab[HASHSIZE];
char params[PARMSIZE];
int par[MAXPARMS];
int np;

int main(argc, argv)
int argc;
char *argv[];
{
	in = UNDEFINED;
	out = UNDEFINED;
	filen = 1;
	eof = FALSE;
	maxdef = deftab + DEFSIZE;
	maxsym = symtab + SYMSIZE;
	inithash();
#ifdef __BBC__
	install("__BBC__","");		/* Set definition for target */
	install("__bbc__","");
	install("__6502__","");
#endif
	while ((*argv[1] & 0xFF) == '-') {	/* BUG in compiler */
		ip = argv[1] + 1;
		if (*ip++ == 'D') {
			if (sym(sbuf) == FALSE)
				usage();
			if (*ip++ == '=')
				install(sbuf, ip);
			else
				install(sbuf, "");
			--argc;
			argv++;
		}
		else
			usage();
	}
	if (argc < 2 | argc > 3)
		usage();
	if ((in = fopen(argv[1], "r")) == NULL)
		cant(argv[1]);
	if (argc == 2)
		out = stdout;
	else {
		if ((out = fopen(argv[2], "w")) == NULL)
			cant(argv[2]);
	}
	parse();
	fclose(out);
	exit(0);
}

/*
 * print correct usage, and exit
 */
usage()
{
	fprintf(stderr, "usage: tcpp [-Dname[=def]] infile [outfile]\n");
	fatal(-1);
}

/*
 * initialise empty hash table
 */
inithash()
{
	int i;

	freeptr = symtab;
	defptr = deftab;
	i = 0;
	while (i < HASHSIZE)
		hashtab[i++] = 0;
}

/*
 * print offending line, and exit
 */
error()
{
	fprintf(stderr, "\n%s", obuf);
	fatal(-1);
}

/*
 * tidy up and exit on fatal error
 */
fatal(stat)
int stat;
{
	if (in != NULL)
		fclose(in);
	if (out != NULL)
		fclose(out);
	exit(stat);
}

/*
 * parse input text
 */
parse()
{
	while (filen) {
		inline();
		if (amatch("#ifdef", 6))
			ifdef();
		else if (amatch("#ifndef", 7))
			ifndef();
		else if (amatch("#else", 5))
			break;
		else if (amatch("#endif", 6))
			break;
		else if (amatch("#define", 7))
			addmac();
		else {
			process();
			rescan();
		}
	}
}

/*
 * rescan processed line looking for preprocessor #directives
 */
rescan()
{
	ip = obuf;
	if (amatch("#include", 8))
		openincl();
	else if (amatch("#if ", 4))
		doif();
	else
		fputs(obuf, out);
}

/*
 * match n chars with input buffer
 */
amatch(s, n)
char *s;
int n;
{
	if (strncmp(ip, s, n) != SAME)
		return(FALSE);
	else {
		ip = ip + n;
		return(TRUE);
	}
}

/*
 * skip 'white space' on input
 */
skip()
{
	while(*ip == ' ' | *ip == '\t')
		ip++;
}

/*
 * open nested #include file
 * -------------------------
 * Tries to open file in current directory first, then tries
 * 'standard' path if #include <name> was used.
 */
openincl()
{
	char delim;			/* filename delimeter */
	char path[LINESIZE];		/* used to prepend 'standard' path */

	if (filen >= 3) {
		fprintf(stderr, "tcpp: too many #include files\n");
		fatal(-1);
	}
	skip();
	delim = *ip;
	if (delim != '"' & delim != '<') {
		fprintf(stderr, "tcpp: can't include %s\n", ip);
		fatal(-1);
	}
	file[filen++] = in;
	if ((in = fopen(filename(sbuf), "r")) == NULL) {
		if (delim == '<') {
			strcpy(path, INCLUDE);
			strcat(path, sbuf);
			if ((in = fopen(path, "r")) != NULL)
				return;
		}
		fixname(sbuf, delim);
	}
}

/*
 * get name of #include file
 */
char *filename(buf)
char *buf;
{
	char *bp;			/* buffer pointer */
	char delim;			/* filename delimeter */

	bp = buf;
	delim = *ip++;
	if (delim == '<')
		delim = '>';
	while (*ip != delim & *ip != '\n')
		*bp++ = *ip++;
	*bp = '\0';
	return(buf);
}

/*
 * try to find #include file by fiddling with filename
 */
fixname(s, delim)
char *s;
char delim;
{
	char *p;			/* filename pointer */
	char path[LINESIZE];		/* used to prepend 'standard' path */

	/* fix BBC Micro filename suffix */
	for (p = s; *p; ++p)
		;
	if (*(p - 2) == '.')
		*(p - 2) = '/';

	/* now try to open the file again */
	if ((in = fopen(s, "r")) == NULL) {
		if (delim == '"')
			cant(s);
		else {
			strcpy(path, INCLUDE);
			strcat(path, s);
			if ((in = fopen(path, "r")) == NULL)
				cant(path);
		}
	}
}

/*
 * input line
 */
inline()
{
	while (filen) {
		if (fgets(ibuf, BUFSIZ, in))
			break;
		else {
			ibuf[0] = '\n';
			ibuf[1] = '\0';
			fclose(in);
			if (--filen == 0)
				eof = TRUE;
			in = file[filen];
		}
	}
	op = obuf;
	ip = ibuf;
}

/*
 * process line of text
 */
process()
{
	while (*ip != '\n') {
		if (sym(sbuf))
			expand(sbuf);
		else if (*ip == '"' | *ip == '\'')
			qtext();
		else if (amatch("/*", 2))
			comment();
		else if (*ip == ' ' | *ip == '\t')
			keepsp();
		else
			*op++ = *ip++;
	}
	*op++ = '\n';
	*op = '\0';
}

/*
 * get next symbol
 * ---------------
 * copy alpha prefixed alphanumeric string from input buffer,
 * and return length of string
 */
sym(p)
char *p;
{
	char *bp;

	bp = p;
	while (isalnum(*ip) | *ip == '_')
		*bp++ = *ip++;
	*bp = '\0';
	return(bp - p);
}

/*
 * expand macro definition or copy symbol
 */
expand(name)
char *name;
{
	char *tag;
	char *p;
	char *to;
	int np;
	int flag;

	flag = 0;
	np = 0;
	if ((tag = hashfind(name)) != NULL) {
		p = mgetw(tag + BODYPTR);
		if (*ip == '(')
			np = parms1();
		to = op;
		while (*p) {
			if (*p <= np) {
				strcpy(op, par[*p - 1]);
				op = op + strlen(par[*p - 1]);
				p++;
				flag = 1;
			}
			else
				*op++ = *p++;
		}
		if (flag == 1) {
			strcpy(op, ip);
			strcpy(ibuf, obuf);
			op = to;
			ip = ibuf + (op - obuf);
		}
	}
	else while (*name)
		*op++ = *name++;
}

/*
 * get parameters for macro
 */
parms1()
{
	int nb;
	char *s;
	char *p;
	char tbuf[BUFSIZ];

	p = params;
	np = 0;
	nb = 0;
	ip++;
	skip();
	while (*ip != ')') {
		s = tbuf;
		while ((nb > 0) | (*ip != ',' & *ip != ')')) {
			if (*ip == '(')
				nb++;
			if (*ip == ')')
				nb--;
			if (*ip == '\n') {
				fprintf(stderr, "tcpp: unexpected newline\n");
				error();
			}
			*s++ = *ip++;
		}
		*s = '\0';
		par[np++] = p;
		strcpy(p, tbuf);
		p = p + strlen(p);
		*p++ = '\0';
		if (np > MAXPARMS) {
			fprintf(stderr, "tcpp: too many parameters\n");
			error();
		}
		if (*ip != ')')
			ip++;
		skip();
	}
	ip++;
	return(np);
}

/*
 * copy quoted text
 * ----------------
 * delimeter is current input char
 * \ character escapes are preserved
 */
qtext()
{
	char delim;

	delim = *op++ = *ip++;
	while (*ip != delim) {
		if ((*op++ = *ip) == '\0') {
			fprintf(stderr, "tcpp: delimeter missing\n");
			error();
		}
		else if (*ip++ == '\\')
			*op++ = *ip++;
	}
	*op++ = *ip++;
	return;
}

/*
 * skip comment
 */
comment()
{
	while (eof == FALSE) {
		if ((inchar() == '*') & (*ip == '/'))
			break;
	}
	++ip;
}

/*
 * keep one space between tokens
 */
keepsp()
{
	*op++ = ' ';
	while (*ip == ' ' | *ip == '\t')
		ip++;
}

/*
 * add new macro to symbol table
 */
addmac()
{
	char *p;

	skip();
	if (sym(name) == 0) {
		fprintf(stderr, "tcpp: illegal symbol name\n");
		error();
	}
	else {
		process();
		ip = obuf;
		macdef(mac);
		install(name, mac);
	}
}

/*
 * #ifdef ... #endif
 */
ifdef()
{
	skip();
	if (sym(sbuf) == 0) {
		fprintf(stderr, "tcpp: '#ifdef symbol' expected\n");
		error();
	}
	if (hashfind(sbuf)) {
		parse();
		ip = ibuf;
		if (amatch("#else", 5)) {
			while (amatch("#endif", 6) == FALSE)
				inline();
		}
	}
	else {
		while (amatch("#endif", 6) == FALSE) {
			if (amatch("#else", 5)) {
				parse();
				break;
			}
			inline();
		}
	}
}

/*
 * #ifndef ... #endif
 */
ifndef()
{
	skip();
	if (sym(sbuf) == 0) {
		fprintf(stderr, "tcpp: '#ifndef symbol' expected\n");
		error();
	}
	if (hashfind(sbuf) == FALSE) {
		parse();
		ip = ibuf;
		if (amatch("#else", 5)) {
			while (amatch("#endif", 6) == FALSE)
				inline();
		}
	}
	else {
		while (amatch("#endif", 6) == FALSE) {
			if (amatch("#else", 5)) {
				parse();
				break;
			}
			inline();
		}
	}
}

/*
 * #if ... #endif
 */
doif()
{
	int n;

	skip();
	if (isalnum(*ip) == FALSE) {
		fprintf(stderr, "tcpp: '#if symbol' expected\n");
		error();
	}
	if ((n = atoi(ip)) != 0) {
		parse();
		ip = ibuf;
		if (amatch("#else", 5)) {
			while (amatch("#endif", 6) == FALSE)
				inline();
		}
	}
	else {
		while (amatch("#endif", 6) == FALSE) {
			if (amatch("#else", 5)) {
				parse();
				break;
			}
			inline();
		}
	}
}

/*
 * read char from input buffer
 * ---------------------------
 * input new line if necessary
 */
inchar()
{
	if (*ip == '\0')
		inline();
	if (eof)
		return(0);
	else
		return(*ip++);
}

/*
 * hashing algorithm
 * -----------------
 * returns value in range 0 to HASHSIZE - 1
 * for best results HASHSIZE should be a prime
 */
hash(name)
char *name;
{
	int h;

	h = 0;
	while (*name)
		h = (3 * h + *name++) % HASHSIZE;
	return(h);
}

/*
 * install new macro
 */
install(name, mac)
char *name;
char *mac;
{
	char *p;
	char *m;
	int len;
	int h;

	len = strlen(name) + 5;
	if (freeptr + len > maxsym) {
		fprintf(stderr, "tcpp: symbol table full\n");
		fatal(-1);
	}
	if (defptr + strlen(mac) > maxdef) {
		fprintf(stderr, "tcpp: macro definition table full\n");
		fatal(-1);
	}
	if (hashfind(name) != NULL)
		printf("tcpp: macro %s redefined\n", name);
	h = hash(name);
	p = freeptr;
	mputw(p + NEXTPTR, hashtab[h]);
	hashtab[h] = p;
	mputw(p + BODYPTR, defptr);
	m = mac;
	while (*m != '\0')
		*defptr++ = *m++;
	*defptr++ = '\0';
	strcpy(p + NAME, name);
	freeptr = p + len;
}

/*
 * get macro definition
 */
macdef(m)
char *m;
{
	int flag;
	int i;
	char tbuf[BUFSIZ];

	np = 0;
	if (*ip == '(')
		parms();
	skip();
	while  (*ip != '\n') {
		if (sym(tbuf) == 0)
			*m++ = *ip++;
		else {
			flag = 1;
			for (i = 0; i < np; ++i) {
				if (strcmp(par[i], tbuf) == SAME) {
					*m++ = i + 1;
					flag = 0;
				}
			}
			if (flag == 1) {
				strcpy(m, tbuf);
				m = m + strlen(m);
			}
		}
	}
	*m = '\0';
}

/*
 * get parameters for macro
 */
parms()
{
	char *p;
	char tbuf[BUFSIZ];

	p = params;
	ip++;
	skip();
	while (*ip != ')') {
		if (sym(tbuf) == 0) {
			fprintf(stderr, "tcpp: illegal parameter %s\n", ip);
			error();
		}
		par[np++] = p;
		strcpy(p, tbuf);
		p = p + strlen(tbuf);
		*p++ = '\0';
		if (np > MAXPARMS) {
			fprintf(stderr, "tcpp: too many parameters\n");
			error();
		}
		skip();
		if (*ip == ',') {
			ip++;
			skip();
		}
	}
	ip++;
}

/*
 * find symbol using hash + chain
 */
char *hashfind(name)
char *name;
{
	char *tag;

	tag = hashtab[hash(name)];
	while (tag) {
		if (strcmp(tag + NAME, name) == SAME)
			break;
		else
			tag = mgetw(tag + NEXTPTR);
	}
	return(tag);
}

/*
 * print can't open ... and exit
 */
cant(s)
char *s;
{
	fprintf(stderr, "tcpp: can't open %s\n", s);
	fatal(-1);
}

/*
 * put word into memory low byte first
 */
mputw(p, val)
int *p;
int *val;
{
	*p = val;
}

/*
 * get word from memory low byte first
 */
mgetw(p)
int *p;
{
	return(*p);
}
