//{{{  main.c - description
/*
  main.h          Declarations for all public global variables, types, and macros.
                  Functions are declared in separate files p2c.{proto,hdrs} which are created
                  mechanically by the makeproto program.
  main.c          Main program.  Parses the p2crc file.  Also reserves storage for public globals in trans.h.

  stuff.c         Miscellaneous support routines.

  out.c           Routines to handle the writing of C code to the output file.
                  This includes line breaking and indentation support.

  comment.c       Routines for managing comments and comment lists.

  lex.c           Lexical analyzer.  Manages input files and streams, splits input stream into Pascal tokens.
                  Parses compiler directives and special comments. Also keeps the symbol table.
  parse.c         Parsing and writing statements and blocks.

  decl.c          Parsing and writing declarations.

  expr.c          Manipulating expressions.

  pexpr.c         Parsing and writing expressions.

  funcs.c         Built-in special functions and procedures.

  dir.c           Interface file to "external" functions and procedures such as hpmods and citmods.

  hpmods.c        Definitions for HP-supplied Pascal modules.

  citmods.c       Definitions for some Caltech-local Pascal modules.
                  (Outside of Caltech this file is mostly useful as a large body of examples of how to write your
                  own translator extensions.)

  p2crc           Control file (read when p2c starts up).

  p2c.h           Header file used by translated programs.
  p2clib.c        Run-time library used by translated programs.
*/
//}}}
//{{{
/* "p2c", a Pascal to C translator.
   Copyright (C) 1989, 1990, 1991, 1992, 1993 Free Software Foundation.
   Author's address: daveg@synaptics.com.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation (any version).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */
//}}}
#define _CRT_SECURE_NO_WARNINGS
#define DEFINE_GLOBALS
#define PROTO_TRANS_C
#include "main.h"
#include <time.h>

//{{{
#if !defined(NO_ISBOGUS) && (defined(mc68000) || defined(m68k) || defined(vax))
   int ISBOGUS(char *p) {
     unsigned long ip = (unsigned long)p;
     if (ip < 0) {
       if (ip < (unsigned long)&ip)
         return 1;    /* below the start of the stack */
       }
     else
       return 1;
     return 0;
     }
#else
  #define ISBOGUS(p) 0
#endif
//}}}

Static int commandLineDebug = 0;
//{{{  static vars
Static Strlist* tweaksymbols;
Static Strlist* synonyms;

Strlist* addmacros;

Static time_t startTime;
//}}}
//{{{
Static void initrc() {

  int i;
  for (i = 0; i < numparams; i++) {
    switch (rctable[i].kind) {
      case 'S':
      case 'B':
        *((short*)rctable[i].ptr) = (short)rctable[i].def;
        break;

      case 'I':
      case 'D':
        *((int*)rctable[i].ptr) = rctable[i].def;
        break;

      case 'L':
        *((long*)rctable[i].ptr) = rctable[i].def;
        break;

      case 'R':
        *((double*)rctable[i].ptr) = rctable[i].def/100.0;
        break;

      case 'U':
      case 'C':
        *((char *)rctable[i].ptr) = 0;
        break;

      case 'A':
        *((Strlist **)rctable[i].ptr) = NULL;
        break;

      case 'X':
        if (rctable[i].def == 1 || rctable[i].def == 4)
          *((Strlist **)rctable[i].ptr) = NULL;
        break;
      }

    rcprevvalues[i] = NULL;
    }

  tweaksymbols = NULL;
  synonyms = NULL;
  addmacros = NULL;
  varmacros = NULL;
  constmacros = NULL;
  fieldmacros = NULL;
  funcmacros = NULL;
  }
//}}}
//{{{
Static int readrc (char* rcname, int need) {

  FILE* rc;
  char buf[500], *cp, *cp2;
  long val = 0;
  int i;
  Strlist* sl;

  rc = fopen (rcname, "r");
  if (!rc) {
    if (need)
      perror (rcname);
    return 0;
    }

  while (fgets (buf, 500, rc)) {
    cp = my_strtok (buf, " =\r\t\n");
    if (cp && *cp != '#') {
      upc (cp);
      i = numparams;
      while (--i >= 0 && strcmp (rctable[i].name, cp));
      if (i >= 0) {
        if (rctable[i].kind != 'M') {
          if (rctable[i].kind == 'X' && rctable[i].def == 4) {
            cp = my_strtokq (NULL, " =\r\t\n");
            }
          else {
            cp = my_strtok (NULL, " =\r\t\n");
            if (cp && *cp == '#')
              cp = NULL;
            }
          if (cp && (isdigit (*cp) || *cp == '-' || *cp == '+'))
            val = atol (cp);
          else
            val = rctable[i].def;
          }

        switch (rctable[i].kind) {
          //{{{
          case 'S':
            *((short*)rctable[i].ptr) = (short)val;
            break;
          //}}}
          //{{{
          case 'I':
            *((int*)rctable[i].ptr) = val;
            break;
          //}}}
          //{{{
          case 'D':
            *((int*)rctable[i].ptr) = parsedelta (cp, rctable[i].def);
            break;
          //}}}
          //{{{
          case 'L':
            *((long*)rctable[i].ptr) = val;
            break;
          //}}}
          //{{{
          case 'R':
            if (cp && (isdigit(*cp) || *cp == '-' || *cp == '.'))
              *((double*)rctable[i].ptr) = atof(cp);
            else
              *((double*)rctable[i].ptr) = rctable[i].def/100.0;
            break;
          //}}}
          //{{{
          case 'U':
            if (cp)
              upc (cp);
          //}}}
          /* fall through */
          //{{{
          case 'C':
            val = rctable[i].def;
            strncpy ((char*)rctable[i].ptr, cp ? cp : "", val-1);
            ((char*)rctable[i].ptr)[val-1] = 0;
            break;
          //}}}
          //{{{
          case 'F':
            while (cp && *cp != '#') {
              sl = strlist_append (&tweaksymbols, format_s("*%s", cp));
              sl->value = rctable[i].def;
              cp = my_strtok (NULL, " \r\t\n");
              }
            break;
          //}}}
          //{{{
          case 'G':
            while (cp && *cp != '#') {
              sl = strlist_append (&tweaksymbols, cp);
              sl->value = rctable[i].def;
              cp = my_strtok (NULL, " \r\t\n");
              }
            break;
          //}}}
          //{{{
          case 'A':
            while (cp && *cp != '#') {
              strlist_insert ((Strlist **)rctable[i].ptr, cp);
              cp = my_strtok (NULL, " \r\t\n");
              }
            break;
          //}}}
          //{{{
          case 'M':
            cp = my_strtok (NULL, "\n");
            if (cp) {
              while (isspace (*cp)) cp++;
              for (cp2 = cp; *cp2 && *cp2 != '#'; cp2++) ;
              *cp2 = 0;
              if (*cp) {
                sl = strlist_append (&addmacros, cp);
                sl->value = rctable[i].def;
                }
              }
            break;
          //}}}
          //{{{
          case 'B':
            if (cp)
              val = parse_breakstr(cp);
            if (val != -1)
              *((short*)rctable[i].ptr) = (short)val;
            break;
          //}}}
          //{{{
          case 'X':
            switch (rctable[i].def) {
              case 1: /* strlist with string values */
              case 4: /* same, allowing quotes */
                if (cp) {
                  sl = strlist_append ((Strlist**)rctable[i].ptr, cp);
                  if (rctable[i].def == 4) {
                    cp = my_strtokq (NULL, " =\r\t\n");
                    }
                  else {
                    cp = my_strtok (NULL, " =\r\t\n");
                    if (cp && *cp == '#')
                      cp = NULL;
                    }
                  if (cp)
                    sl->value = (int64_t)stralloc (cp);
                  }
                 break;

               case 2: /* Include */
                 if (cp)
                   readrc (format_s (cp, infname), 1);
                 break;

               case 3: /* Synonym */
                 if (cp) {
                   sl = strlist_append (&synonyms, cp);
                   cp = my_strtok (NULL, " =\r\t\n");
                   if (cp && *cp != '#')
                     sl->value = (int64_t)stralloc (cp);
                   }
                 break;
               }
          //}}}
          }
        }
      else
        fprintf (stderr, "warning: can't understand %s in %s\n", cp, rcname);
      }
    }

  fclose (rc);
  return 1;
  }
//}}}
//{{{
Static void postrc() {

  int longbits;
  unsigned long val;

  which_unix = UNIX_ANY;
  if (!strcmp(target, "CHIPMUNK") ||
      !strcmp(target, "HPUX-300") ||
      !strcmp(target, "SUN-68K") ||
      !strcmp(target, "BSD-VAX")) {
    signedchars = 1;
    sizeof_char = 8;
    sizeof_short = 16;
    sizeof_int = sizeof_long = sizeof_pointer = 32;
    sizeof_enum = 32;
    sizeof_float = 32;
    sizeof_double = 64;

    if (!strcmp(target, "CHIPMUNK") || !strcmp(target, "HPUX-300"))
      which_unix = UNIX_SYSV;
    else
      which_unix = UNIX_BSD;
    }

  else if (!strcmp (target, "LSC-MAC")) {
    signedchars = 1;
    if (prototypes < 0)
      prototypes = 1;
    if (fullprototyping < 0)
      fullprototyping = 0;
    if (voidstar < 0)
      voidstar = 1;
    sizeof_char = 8;
    sizeof_short = sizeof_int = 16;
    sizeof_long = sizeof_pointer = 32;
    }

  else if (!strcmp (target, "BSD")) {
    which_unix = UNIX_BSD;
    }

  else if (!strcmp (target, "SYSV")) {
    which_unix = UNIX_SYSV;
    }

  else if (*target) {
    fprintf(stderr, "p2c: warning: don't understand target name %s\n", target);
    }

  if (ansiC > 0) {
    if (sprintf_value < 0)
      sprintf_value = 0;
    if (castnull < 0)
      castnull = 0;
    }

  if (useenum < 0)
    useenum = (ansiC != 0) ? 1 : 0;
  if (void_args < 0)
    void_args = (ansiC > 0 && prototypes != 0) ? 1 : 0;
  if (prototypes < 0)
    prototypes = (cplus > 0) ? 2 : (ansiC > 0) ? 1 : 0;
  if (prototypes == 0)
    fullprototyping = 0;
  else if (fullprototyping < 0)
    fullprototyping = 1;
  if (useAnyptrMacros < 0)
    useAnyptrMacros = (ansiC > 0 || cplus > 0) ? 2 : 1;
  if (usePPMacros < 0)
    usePPMacros = (ansiC > 0 || cplus > 0) ? 0 : 2;
  if (newdelete < 0)
    newdelete = (cplus > 0) ? 1 : 0;
  if (voidstar < 0)
    voidstar = (ansiC > 0 || cplus > 0) ? 1 : 0;
  if (hassignedchar < 0)
    hassignedchar = (ansiC > 0) ? 1 : 0;
  if (useconsts < 0)
    useconsts = (ansiC > 0 || cplus > 0) ? 1 : 0;
  if (slashslash < 0)
    slashslash = (cplus > 0) ? 1 : 0;
  if (userefs < 0)
    userefs = (cplus > 0) ? 1 : 0;
  if (useinits < 0)
    useinits = (cplus > 0) ? 4 : 1;
  if (anonymousunions < 0)
    anonymousunions = (cplus > 0) ? 1 : 0;
  if (callcasts < 0)
    callcasts = (cplus > 0) ? 1 : 0;
  if (copystructs < 0)
    copystructs = (ansiC != 0 || cplus > 0) ? 3 : 0;
  if (copystructfuncs < 0)
    copystructfuncs = (ansiC > 0 || cplus > 0) ? 0 : 1;
  if (starfunctions < 0)
    starfunctions = (ansiC > 0) ? 0 : 1;
  if (variablearrays < 0)
    variablearrays = (ansiC > 1) ? 1 : 0;
  if (initpacstrings < 0)
    initpacstrings = (ansiC > 0) ? 1 : 0;
  if (*memcpyname) {
    if (ansiC > 0 || which_unix == UNIX_SYSV)
      strcpy(memcpyname, "memcpy");
    else if (which_unix == UNIX_BSD)
      strcpy(memcpyname, "bcopy");
    }

  sizeof_integer = (sizeof_int >= 32) ? sizeof_int : sizeof_long;
  integername = (sizeof_int >= 32) ? "int" : "long";
  if (sizeof_integer && sizeof_integer < 32)
    fprintf(stderr, "Warning: long integers have less than 32 bits\n");
  if (sizeof_int >= 32 && sizeof_long > sizeof_int && prototypes == 0)
    fprintf(stderr, "Warning: translated code assumes int and long are the same");
  if (setbits < 0)
    setbits = (sizeof_integer > 0) ? sizeof_integer : 32;

  ucharname = (*name_UCHAR) ? name_UCHAR : (signedchars == 0) ? "char" : "unsigned char";
  scharname = (*name_SCHAR) ? name_SCHAR :
                (signedchars == 1) ? "char" :
                  (useAnyptrMacros == 1) ? "Signed char" : "signed char";

  for (longbits = 1, val = LONG_MAX; (val >>= 1); longbits++) ;
  if (sizeof_char) {
    if (sizeof_char < 8 && ansiC > 0)
      fprintf(stderr, "Warning: chars have less than 8 bits\n");
    if (sizeof_char > longbits) {
      min_schar = LONG_MIN;
      max_schar = LONG_MAX;
      }
    else {
      min_schar = - (1<<(sizeof_char-1));
      max_schar = (1<<(sizeof_char-1)) - 1;
      }
    if (sizeof_char >= longbits)
      max_uchar = LONG_MAX;
    else
      max_uchar = (1<<sizeof_char) - 1;
    }
  else {
    min_schar = -128;      /* Ansi-required minimum maxima */
    max_schar = 127;
    max_uchar = 255;
    }

  if (sizeof_short) {
    if (sizeof_short < 16 && ansiC > 0)
      fprintf (stderr, "Warning: shorts have less than 16 bits\n");
    if (sizeof_short > longbits) {
      min_sshort = LONG_MIN;
      max_sshort = LONG_MAX;
      }
    else {
      min_sshort = - (1L<<(sizeof_short-1));
      max_sshort = (1L<<(sizeof_short-1)) - 1;
      }
    if (sizeof_short >= longbits)
      max_ushort = LONG_MAX;
    else
      max_ushort = (1L<<sizeof_short) - 1;
    }
  else {
    min_sshort = -32768;   /* Ansi-required minimum maxima */
    max_sshort = 32767;
    max_ushort = 65535;
    }

  if (symcase < 0)
    symcase = 1;
  if (smallsetconst == -2)
    smallsetconst = (*name_SETBITS) ? -1 : 1;

  hpux_lang = 0;
  if (!strcmp (language, "TURBO")) {
    which_lang = LANG_TURBO;
    }
  else if (!strcmp (language, "UCSD")) {
    which_lang = LANG_UCSD;
    }
  else if (!strcmp (language, "MPW") || !strcmp (language, "OBJECT")) {
    which_lang = LANG_MPW;
    }
  else if (!strcmp (language, "HPUX") || !strcmp (language, "HP-UX")) {
    which_lang = LANG_HP;
    hpux_lang = 1;
    }
  else if (!strcmp (language, "OREGON")) {
    which_lang = LANG_OREGON;
    }
  else if (!strcmp (language, "VAX") || !strcmp (language, "VMS")) {
    which_lang = LANG_VAX;
    }
  else if (!strncmp (language, "MODULA", 6)) {
    which_lang = LANG_MODULA;
    }
  else if (!strncmp (language, "BERK", 4) ||
    !strcmp(language, "SUN")) {
    which_lang = LANG_BERK;
    }
  else if (!strcmp (language, "TIP")) {
    which_lang = LANG_TIP;
    }
  else if (!strcmp (language, "APOLLO")) {
    which_lang = LANG_APOLLO;
    }
  else {
    if (*language && strcmp  (language, "HP") && strcmp (language, "MODCAL"))
      fprintf (stderr, "Warning: Language %s not recognized, using HP\n", language);
    which_lang = LANG_HP;
    }

  if (modula2 < 0)
    modula2 = (which_lang == LANG_MODULA) ? 1 : 0;
  if (pascalcasesens < 0)
    pascalcasesens = (which_lang == LANG_MODULA) ? 2 :
                       (which_lang == LANG_BERK) ? 3 : 0;

  if (implementationmodules < 0)
    implementationmodules = (which_lang == LANG_VAX ||
      which_lang == LANG_APOLLO || which_lang == LANG_BERK) ? 1 : 0;

  if (integer16 < 0)
    integer16 = (which_lang == LANG_TURBO || which_lang == LANG_MPW || which_lang == LANG_TIP) ? 1 : 0;
  if (doublereals < 0)
    doublereals = (hpux_lang || which_lang == LANG_OREGON || which_lang == LANG_VAX ||
       which_lang == LANG_TIP) ? 0 : 1;

  if (pascalenumsize < 0)
    pascalenumsize = (which_lang == LANG_HP) ? 16 : 8;

  if (storefilenames < 0)
    storefilenames = (which_lang == LANG_TURBO) ? 1 : 0;
  if (charfiletext < 0)
    charfiletext = (which_lang == LANG_BERK) ? 1 : 0;
  if (readwriteopen < 0)
    readwriteopen = (which_lang == LANG_TURBO) ? 1 : 0;
  if (literalfilesflag < 0)
    literalfilesflag = (which_lang == LANG_BERK) ? 2 : 0;
  if (newlinespace < 0)
        newlinespace = (which_lang == LANG_TURBO) ? 0 : 1;

  if (nestedcomments < 0)
    nestedcomments = (which_lang == LANG_TURBO || which_lang == LANG_MPW ||
                      which_lang == LANG_UCSD || which_lang == LANG_BERK) ? 2 : 0;

  if (importall < 0)
    importall = (which_lang == LANG_HP) ? 1 : 0;
  if (turboobjects < 0)
    turboobjects = (which_lang == LANG_MPW) ? 0 : 1;
  if (seek_base < 0)
    seek_base = (which_lang == LANG_TURBO || which_lang == LANG_MPW ||
                 which_lang == LANG_UCSD || which_lang == LANG_TIP) ? 0 : 1;

  if (lowpreclogicals < 0)
    lowpreclogicals = (which_lang == LANG_TIP);
  if (unsignedchar < 0 && signedchars == 0)
    unsignedchar = 2;
  if (hasstaticlinks < 0)
    hasstaticlinks = (which_lang == LANG_HP) ? 1 : 0;
  if (dollar_idents < 0)
    dollar_idents = (which_lang == LANG_OREGON || which_lang == LANG_VAX || which_lang == LANG_TIP) ? 1 : 0;
  if (ignorenonalpha < 0)
    ignorenonalpha = (which_lang == LANG_UCSD) ? 1 : 0;
  if (stringtrunclimit < 0)
    stringtrunclimit = (which_lang == LANG_TURBO) ? 80 : 0;
  if (defaultsetsize < 0)
    defaultsetsize = (which_lang == LANG_VAX) ? 256 :
     (which_lang == LANG_BERK) ? 128 :
       (which_lang == LANG_MPW) ? 2040 : 8192;
  if (enumbyte < 0)
    enumbyte = (which_lang == LANG_HP) ? 0 : 1;

  if (!*filenamefilter && (which_lang == LANG_OREGON || which_lang == LANG_BERK))
    strcpy (filenamefilter, "P_trimname");

  charname = (useAnyptrMacros) ? "Char" :
               (unsignedchar == 1) ? ucharname :
                 (unsignedchar == 0) ? scharname : "char";

  if (!*memcpyname)
    strcpy (memcpyname, "memcpy");

  if (!*mallocname)
    strcpy (mallocname, "malloc");

  if (!*freename)
    strcpy (freename, "free");

  fix_parameters();
  }
//}}}

//{{{
Static void openlogfile() {

  char* name;
  if (*codefname == '<')
    name = format_ss (logfnfmt, infname, infname);
  else
    name = format_ss (logfnfmt, infname, codefname);
  if (!name)
    name = format_s ("%s.log", codefname);

  logfile = fopen (name, "w");
  if (logfile) {
    fprintf (logfile, "p2c %s to %s by version %s\n", infname, codefname, P2C_VERSION);

    time (&startTime);
    fprintf (logfile, "started %s\n", ctime (&startTime));
    }

  else {
    perror (name);
    verbose = 0;
    }
  }
//}}}
//{{{
void closelogfile() {

  if (logfile) {
    time_t endTime;
    time (&endTime);
    fprintf (logfile, "processed %d source lines in %ld:%ld seconds\n",
                       inf_ltotal, ((long)endTime - (long)startTime) / 60,
                       ((long)endTime - (long)startTime) % 60);
    fprintf (logfile, "finished %s\n", ctime (&endTime));
    fclose (logfile);
    }
  }
//}}}

//{{{
void exit_failure() {
  exit(EXIT_FAILURE);
  }
//}}}
//{{{
void showinitfile() {

  char* name = format_s ("%s", "p2crc");
  printf ("p2crc %s:\n", name);

  FILE* f = fopen (name, "r");
  if (!f) {
    perror (name);
    exit_failure();
    }

  int ch;
  while ((ch = getc(f)) != EOF)
    putchar (ch);

  fclose (f);
  }
//}}}

//{{{
char* meaningkindname (enum meaningkind kind) {

  if ((unsigned int)kind < (unsigned int)MK_LAST)
    return meaningkindnames[(int)kind];
  else
    return format_d ("<meaning %d>", (int)kind);
  }
//}}}
//{{{
char* typekindname (enum typekind kind) {

  if ((unsigned int)kind < (unsigned int)TK_LAST)
    return typekindnames[(int)kind];
  else
    return format_d ("<type %d>", (int)kind);
  }
//}}}
//{{{
char* exprkindname (enum exprkind kind) {

  if ((unsigned int)kind < (unsigned int)EK_LAST)
    return exprkindnames[(int)kind];
  else
    return format_d ("<expr %d>", (int)kind);
}
//}}}
//{{{
char* stmtkindname (enum stmtkind kind) {

  if ((unsigned int)kind < (unsigned int)SK_LAST)
    return stmtkindnames[(int)kind];
  else
    return format_d ("<stmt %d>", (int)kind);
  }
//}}}

//{{{
void usage() {
  fprintf (stderr, "usage: p2c [-q -t -x -e -v -check] file\n");
  exit_failure();
  }
//}}}
//{{{
int main (int argc, char** argv) {

  if (commandLineDebug) {
    //{{{  commandLineDebug
    printf ("p2c commandLine parsed as");
    for (int i = 0; i < argc; i++)
      printf (" %d:%s", i, argv[i]);
    printf ("\n");
    }
    //}}}

  int numsearch;
  char codefnbuf[200];
  char hdrfnbuf[200];
  Symbol* sp;
  Strlist* sl;
  int nobuffer = 0;

  init_stuff();

  initrc();
  setup_dir();

  // skip past switches till infname text
  char infnbuf[200];
  infname = infnbuf;
  *infname = 0;
  int i = 1;
  while ((i < argc) && (argv[i][0] == '-'))
    i++;
  if (i < argc)
    strcpy (infname, argv[i]);

  readrc ("options.txt", 1);

  codefname = codefnbuf;
  *codefname = 0;

  hdrfname = hdrfnbuf;
  *hdrfname = 0;

  requested_module = NULL;
  found_module = 0;
  error_crash = 0;
  conserve_mem = 1;
  regression = 0;
  verbose = 0;
  partialdump = 1;
  numsearch = 0;

  //{{{  option parser
  argc--;
  argv++;

  while (argc > 0) {
    if (**argv == '-' && (*argv)[1]) {
      // options
      if (!strcmp(*argv, "-q"))
        quietmode = 1;

      else if (!strcmp (*argv, "-t"))
        tokentrace++;

      else if (!strcmp (*argv, "-x"))
        error_crash++;

      else if (!strcmp(*argv, "-c")) {
        /* Enable all error checking */
        casecheck = 1;
        arraycheck = 1;
        nilcheck = 1;
        malloccheck = 1;
        checkfileisopen = 1;
        checkreadformat = 1;
        checkfileeof = 1;
        checkstdineof = 1;
        checkfileseek = 1;
        }

      else if (argv[0][1] == 'e') {
        if (strlen (*argv) == 2)
          maxerrors = 0;
        else
          maxerrors = atoi (*argv + 2);
        }

      else if (argv[0][1] == 'v') {
        if (strlen (*argv) == 2)
          verbose = 1;
        else
          verbose = atoi (*argv + 2);
        }

      else
        usage();
      }

    argc--;
    argv++;
    }
  //}}}

  if (requested_module && !*codefname)
    strcpy (codefname, format_ss (modulefnfmt, infname, requested_module));

  if (*infname && strcmp (infname, "-")) {
    //{{{  check for and open infname file for read
    if (strlen (infname) > 2 &&
        !strcmp (infname + strlen(infname) - 2, ".c")) {
      fprintf (stderr, "What is wrong with this picture?\n");
      exit_failure();
      }

    if (!quietmode)
      printf ("using pascal %s\n", infname);

    inf = fopen (infname, "r");
    if (!inf) {
      perror (infname);
      exit_failure();
      }

    if (!*codefname)
      strcpy (codefname, format_s (codefnfmt, infname));
    }
    //}}}
  else
    exit_failure();

  if (!quietmode)
    printf ("using options.txt\n");

  if (strcmp (codefname, "-")) {
    //{{{  open codefname file for write
    codef = fopen (codefname, "w");
    if (!codef) {
      perror (codefname);
      exit_failure();
      }

    i = (slashslash > 0 || (slashslash < 0 && cplus > 0));
    fprintf (codef, i ? "// " : "/* ");
    fprintf (codef, "Output from p2c %s, the Pascal-to-C translator", P2C_VERSION);
    fprintf (codef, i ? "\n" : " */\n");
    }
    //}}}

  if (nobuffer)
    setbuf (codef, NULL); // for debugging

  outf = codef;
  outf_lnum = 1;

  logfile = NULL;
  if (verbose)
    openlogfile();

  setup_complete = 0;
  init_lex();
  leadingcomments();

  postrc();
  setup_comment();  // must call this first
  setup_lex();      // must call this second
  setup_out();
  setup_decl();     // must call *after* setup_lex()
  setup_parse();
  setup_funcs();

  for (sl = tweaksymbols; sl; sl = sl->next) {
    char* cp = sl->s;
    if (*cp == '*') {
      cp++;
      if (!pascalcasesens)
        upc (cp);
      }

    sp = findsymbol (cp);
    if (sl->value & FUNCBREAK)
      sp->flags &= ~FUNCBREAK;
    sp->flags |= sl->value;
    }

  strlist_empty (&tweaksymbols);
  for (sl = synonyms; sl; sl = sl->next) {
    if (!pascalcasesens)
      upc (sl->s);
    sp = findsymbol (sl->s);
    sp->flags |= SSYNONYM;
    if (sl->value) {
      if (!pascalcasesens)
        upc ((char *)sl->value);
      strlist_append (&sp->symbolnames, "===")->value = (int64_t)findsymbol ((char*)sl->value);
      }
    else
      strlist_append (&sp->symbolnames, "===")->value = 0;
    }

  strlist_empty (&synonyms);
  for (sl = addmacros; sl; sl = sl->next)
    defMacro (sl->s, sl->value, "<macro>", 0);

  strlist_empty (&addmacros);
  handle_nameof();
  setup_complete = 1;

  // load import system.imp
 for (sl = librfiles; sl; sl = sl->next)
    if (strlist_find (librfiles, sl->s) == sl)
      p_search (format_none(sl->s), "pas", 0);

  p_program();
  end_source();
  flushcomments (NULL, -1, -1);
  showendnotes();

  check_unused_macros();

  if (!showprogress && !quietmode)
    fprintf (stderr, "\n");

  if (codef != stdout) {
    if (slashslash)
      output ("// End\n");
    else
      output ("/* End */\n");
    }

  // shutdown
  if (inf != stdin)
    fclose (inf);
  if (codef != stdout)
    fclose (codef);

  closelogfile();

  if (!quietmode)
    fprintf (stderr, "p2c finished\n");

  return EXIT_SUCCESS;
  }
//}}}
