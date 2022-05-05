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
/* Roadmap:
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
#if !defined(NO_ISBOGUS) && (defined(mc68000) || defined(m68k) || defined(vax))
   int ISBOGUS(p)
   char *p;
     {
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
//{{{
#ifndef __STDC__
  #ifdef NO_GETENV
    #define getenv(x) NULL
  #else
    extern char *getenv PP((char *));
  #endif
#endif
//}}}

Static Strlist* tweaksymbols;
Static Strlist* synonyms;
Strlist* addmacros;

//{{{
Static void initrc() {

  int i;
  for (i = 0; i < numparams; i++) {
    switch (rctable[i].kind) {
      case 'S':
      case 'B':
        *((short *)rctable[i].ptr) = rctable[i] .def;
        break;

      case 'I':
      case 'D':
        *((int *)rctable[i].ptr) = rctable[i].def;
        break;

      case 'L':
        *((long *)rctable[i].ptr) = rctable[i].def;
        break;

      case 'R':
        *((double *)rctable[i].ptr) = rctable[i].def/100.0;
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
Static int readrc (rcname, need)
char *rcname;
int need;
  {
  FILE *rc;
  char buf[500], *cp, *cp2;
  long val = 0;
  int i;
  Strlist *sl;

  printf ("reading resource from %s\n", rcname);

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
            *((short*)rctable[i].ptr) = val;
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
              *((short*)rctable[i].ptr) = val;
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
                    sl->value = (long)stralloc (cp);
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
                     sl->value = (long)stralloc (cp);
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

Static long starting_time;
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
    fprintf (logfile, "\nTranslation of %s to %s by p2c %s\n", infname, codefname, P2C_VERSION);
    fprintf (logfile, "Translated");

    time (&starting_time);
    fprintf (logfile, " on %s", ctime(&starting_time));
    fprintf (logfile, "\n");
    }

  else {
    perror (name);
    verbose = 0;
    }
  }
//}}}
//{{{
void closelogfile() {

  long ending_time;

  if (logfile) {
    fprintf (logfile, "\n\n");

    time (&ending_time);
    fprintf (logfile, "Processed %d source lines in %ld:%ld seconds.\n",
      inf_ltotal, (ending_time - starting_time) / 60,
      (ending_time - starting_time) % 60);

    fprintf (logfile, "\n\nTranslation completed on %s", ctime(&ending_time));

    fclose (logfile);
    }
  }
//}}}
//{{{
void showinitfile() {

  FILE *f;
  int ch;
  char *name;

  name = format_s ("%s", "p2crc");
  printf ("p2crc %s:\n", name);
  f = fopen (name, "r");
  if (!f) {
    perror (name);
    exit_failure();
    }

  while ((ch = getc(f)) != EOF)
    putchar (ch);
  fclose (f);
  }
//}}}
//{{{
void usage() {

  fprintf(stderr, "usage: p2c [options] file [modulename] [-h file.h] [-o file.c]\n");
  exit_failure();
  }
//}}}

//{{{
void exit_failure() {
  exit(EXIT_FAILURE);
  }
//}}}
//{{{
int outmem() {
  fprintf (stderr, "p2c: Out of memory!\n");
  exit (EXIT_FAILURE);
  }
//}}}

//{{{
char* meaningkindname (kind)
enum meaningkind kind;
{
  #ifdef HASDUMPS
    if ((unsigned int)kind < (unsigned int)MK_LAST)
      return meaningkindnames[(int) kind];
    else
  #endif /*HASDUMPS*/
      return format_d("<meaning %d>", (int) kind);
  }
//}}}
//{{{
char* typekindname (kind)
enum typekind kind;
{
  #ifdef HASDUMPS
    if ((unsigned int)kind < (unsigned int)TK_LAST)
      return typekindnames[(int) kind];
    else
  #endif /*HASDUMPS*/
    return format_d("<type %d>", (int) kind);
}
//}}}
//{{{
char* exprkindname (kind)
enum exprkind kind;
{
  #ifdef HASDUMPS
    if ((unsigned int)kind < (unsigned int)EK_LAST)
        return exprkindnames[(int) kind];
    else
  #endif /*HASDUMPS*/
    return format_d("<expr %d>", (int) kind);
}
//}}}
//{{{
char* stmtkindname (kind)
enum stmtkind kind;
{
  #ifdef HASDUMPS
    if ((unsigned int)kind < (unsigned int)SK_LAST)
        return stmtkindnames[(int) kind];
    else
  #endif /*HASDUMPS*/
    return format_d("<stmt %d>", (int) kind);
}
//}}}

//{{{
void dumptype (tp)
Type *tp;
  {
  if (!tp) {
    fprintf (outf, "<NULL>\n");
    return;
    }

  if (ISBOGUS (tp)) {
    fprintf (outf, "0x%lX\n", (long)tp);
    return;
    }

  fprintf (outf, "      Type %lx, kind=%s", (long)tp, typekindname(tp->kind));

  #ifdef HASDUMPS
    fprintf (outf, ", meaning=%lx, basetype=%lx, indextype=%lx\n",
             (long)tp->meaning, (long)tp->basetype, (long)tp->indextype);
    tp->dumped = 1;
    if (tp->basetype)
      dumptype (tp->basetype);
    if (tp->indextype)
      dumptype (tp->indextype);
  #else
    fprintf (outf, "\n");
  #endif /*HASDUMPS*/
  }
//}}}
//{{{
void dumpmeaning (mp)
Meaning *mp;
{
  if (!mp) {
    fprintf (outf, "<NULL>\n");
    return;
    }

  if (ISBOGUS(mp)) {
    fprintf (outf, "0x%lX\n", (long)mp);
    return;
    }

  fprintf (outf, "   Meaning %lx, name=%s, kind=%s",
           (long)mp, ((mp->name) ? mp->name : "<null>"), meaningkindname(mp->kind));

  #ifdef HASDUMPS
    fprintf (outf, ", ctx=%lx, cbase=%lx, cnext=%lx, type=%lx\n",
             (long)mp->ctx, (long)mp->cbase, (long)mp->cnext, (long)mp->type);
    if (mp->type && !mp->type->dumped)
      dumptype (mp->type);
    mp->dumped = 1;
  #else
    fprintf (outf, "\n");
  #endif
}
//}}}
//{{{
void dumpsymtable (sym)
Symbol *sym;
{
  Meaning *mp;

  if (sym) {
    dumpsymtable(sym->left);
    #ifdef HASDUMPS
      if ((sym->mbase && !sym->mbase->dumped) || (sym->fbase && !sym->fbase->dumped))
    #endif
      {
      fprintf (outf, "Symbol %s:\n", sym->name);
      for (mp = sym->mbase; mp; mp = mp->snext)
        dumpmeaning (mp);
      for (mp = sym->fbase; mp; mp = mp->snext)
        dumpmeaning (mp);
      fprintf (outf, "\n");
      }
  dumpsymtable (sym->right);
  }
}
//}}}
//{{{
void dumptypename (tp, waddr)
Type *tp;
int waddr;
{
  if (!tp) {
    fprintf(outf, "<NULL>");
    return;
    }

  if (ISBOGUS(tp)) {
    fprintf(outf, "0x%lX", (long)tp);
    return;
    }

  if (tp == tp_int)             fprintf(outf, "I");
  else if (tp == tp_sint)       fprintf(outf, "SI");
  else if (tp == tp_uint)       fprintf(outf, "UI");
  else if (tp == tp_integer)    fprintf(outf, "L");
  else if (tp == tp_unsigned)   fprintf(outf, "UL");
  else if (tp == tp_char)       fprintf(outf, "C");
  else if (tp == tp_schar)      fprintf(outf, "UC");
  else if (tp == tp_uchar)      fprintf(outf, "SC");
  else if (tp == tp_boolean)    fprintf(outf, "B");
  else if (tp == tp_longreal)   fprintf(outf, "R");
  else if (tp == tp_real)       fprintf(outf, "F");
  else if (tp == tp_anyptr)     fprintf(outf, "A");
  else if (tp == tp_void)       fprintf(outf, "V");
  else if (tp == tp_text)       fprintf(outf, "T");
  else if (tp == tp_bigtext)    fprintf(outf, "BT");
  else if (tp == tp_sshort)     fprintf(outf, "SS");
  else if (tp == tp_ushort)     fprintf(outf, "US");
  else if (tp == tp_abyte)      fprintf(outf, "AB");
  else if (tp == tp_sbyte)      fprintf(outf, "SB");
  else if (tp == tp_ubyte)      fprintf(outf, "UB");
  else if (tp == tp_str255)     fprintf(outf, "S");
  else if (tp == tp_strptr)     fprintf(outf, "SP");
  else if (tp == tp_charptr)    fprintf(outf, "CP");
  else if (tp == tp_smallset)   fprintf(outf, "SMS");
  else if (tp == tp_proc)       fprintf(outf, "PR");
  else if (tp == tp_jmp_buf)    fprintf(outf, "JB");
  else {
    if (tp->meaning && !ISBOGUS(tp->meaning) &&
        tp->meaning->name && !ISBOGUS(tp->meaning->name) &&
        tp->meaning->name[0]) {
      fprintf(outf, "%s", tp->meaning->name);
      if (tp->dumped)
        return;
      fprintf(outf, "=");
      waddr = 1;
      }

    if (waddr) {
      fprintf(outf, "%lX", (long)tp);
      if (tp->dumped)
        return;
      fprintf(outf, ":");
      tp->dumped = 1;
      }

    switch (tp->kind) {
      //{{{
      case TK_STRING:
        fprintf(outf, "Str");
        if (tp->structdefd)
          fprintf(outf, "Conf");
        break;
      //}}}
      //{{{
      case TK_SUBR:
        dumptypename(tp->basetype, 0);
        break;
      //}}}
      //{{{
      case TK_POINTER:
        fprintf(outf, "^");
        dumptypename(tp->basetype, 0);
        break;
      //}}}

      //{{{
      case TK_SMALLARRAY:
        fprintf(outf, "Sm");
      //}}}
        /* fall through */
      //{{{
      case TK_ARRAY:
        fprintf(outf, "Ar");
        if (tp->structdefd)
          fprintf(outf, "Conf");
        fprintf(outf, "{");
        dumptypename(tp->indextype, 0);
        fprintf(outf, "}");
        if (tp->smin) {
          fprintf(outf, "Skip(");
          dumpexpr(tp->smin);
          fprintf(outf, ")");
          }
        if (tp->smax) {
          fprintf(outf, "/");
          if (!ISBOGUS(tp->smax))
            dumptypename(tp->smax->val.type, 0);
          fprintf(outf, "{%d%s}", tp->escale,
          tp->issigned ? "S" : "U");
          }
        fprintf(outf, ":");
        dumptypename(tp->basetype, 0);
        break;
      //}}}

      //{{{
      case TK_SMALLSET:
        fprintf(outf, "Sm");
      //}}}
        /* fall through */
      //{{{
      case TK_SET:
        fprintf(outf, "Set{");
        dumptypename(tp->indextype, 0);
        fprintf(outf, "}");
        break;
      //}}}

      //{{{
      case TK_FILE:
        fprintf(outf, "File{");
        dumptypename(tp->basetype, 0);
        fprintf(outf, "}");
        break;
      //}}}
      //{{{
      case TK_BIGFILE:
        fprintf(outf, "BigFile{");
        dumptypename(tp->basetype, 0);
        fprintf(outf, "}");
        break;
      //}}}
      //{{{
      case TK_FUNCTION:
        fprintf(outf, "Func");
        if (tp->issigned)
          fprintf(outf, "Link");
        fprintf(outf, "{");
        dumptypename(tp->basetype, 0);
        fprintf(outf, "}");
        break;
      //}}}

      //{{{
      case TK_CPROCPTR:
        fprintf(outf, "C");
      //}}}
      /* fall through */
      //{{{
      case TK_PROCPTR:
        fprintf(outf, "Proc%d{", tp->escale);
        dumptypename(tp->basetype, 0);
        fprintf(outf, "}");
        break;
      //}}}

      //{{{
      default:
        fprintf(outf, "%s", typekindname(tp->kind));
        break;
      //}}}
      }

    if (tp->kind != TK_ARRAY && tp->kind != TK_SMALLARRAY && (tp->smin || tp->smax)) {
      fprintf(outf, "{");
      dumpexpr(tp->smin);
      fprintf(outf, "..");
      dumpexpr(tp->smax);
      fprintf(outf, "}");
      }
    }
}
//}}}
//{{{
void dumptypename_file (f, tp)
FILE *f;
Type *tp;
{
  FILE *save = outf;
  outf = f;
  dumptypename(tp, 1);
  outf = save;
}
//}}}
//{{{
void dumpexpr (ex)
Expr *ex;
{
  int i;
  Type *type;
  char *name;

  if (!ex) {
    fprintf(outf, "<NULL>");
    return;
    }
  if (ISBOGUS(ex)) {
    fprintf(outf, "0x%lX", (long)ex);
    return;
    }

  if (ex->kind == EK_CONST && ex->val.type == tp_integer && ex->nargs == 0 && !ex->val.s) {
    fprintf(outf, "%ld", ex->val.i);
    return;
    }
  if (ex->kind == EK_LONGCONST && ex->val.type == tp_integer && ex->nargs == 0 && !ex->val.s) {
    fprintf(outf, "%ldL", ex->val.i);
    return;
    }

  name = exprkindname(ex->kind);
  if (!strncmp(name, "EK_", 3))
    name += 3;
  fprintf(outf, "%s", name);

#ifdef HASDUMPS
  type = ex->val.type;
  fprintf(outf, "/");
  dumptypename(type, 1);
  if (ex->val.i) {
    switch (ex->kind) {
          case EK_VAR:
          case EK_FUNCTION:
          case EK_CTX:
            if (ISBOGUS(ex->val.i))
              fprintf(outf, "[0x%lX]", ex->val.i);
            else
              fprintf(outf, "[\"%s\"]", ((Meaning *)ex->val.i)->name);
            break;

          default:
            fprintf(outf, "[i=%ld]", ex->val.i);
            break;
      }
    }

  if (ISBOGUS(ex->val.s))
    fprintf(outf, "[0x%lX]", (long)ex->val.s);
  else if (ex->val.s) {
    switch (ex->kind) {
      case EK_BICALL:
      case EK_NAME:
      case EK_DOT:
        fprintf(outf, "[s=\"%s\"]", ex->val.s);
        break;

      default:
        switch (ex->val.type ? ex->val.type->kind : TK_VOID) {
          case TK_STRING:
            fprintf(outf, "[s=%s]", makeCstring(ex->val.s, ex->val.i));
            break;
          case TK_REAL:
            fprintf(outf, "[s=%s]", ex->val.s);
            break;
          default:
            fprintf(outf, "[s=%lx]", (long)ex->val.s);
          }
        break;
      }
    }

  if (ex->nargs > 0) {
    fprintf(outf, "(");
    if (ex->nargs < 10) {
      for (i = 0; i < ex->nargs; i++) {
        if (i)
          fprintf(outf, ", ");
        dumpexpr(ex->args[i]);
        }
      }
    else
      fprintf(outf, "...");
    fprintf(outf, ")");
    }
#endif
}
//}}}
//{{{
void dumpexpr_file (f, ex)
FILE *f;
Expr *ex;
{
  FILE *save = outf;
  outf = f;
  dumpexpr(ex);
  outf = save;
}
//}}}
//{{{
void innerdumpstmt (sp, indent)
Stmt *sp;
int indent;
{
#ifdef HASDUMPS
  if (!sp) {
    fprintf(outf, "<NULL>\n");
    return;
    }

  while (sp) {
    if (ISBOGUS(sp)) {
      fprintf(outf, "0x%lX\n", (long)sp);
      return;
      }
    fprintf(outf, "%s", stmtkindname(sp->kind));
    if (sp->exp1) {
      fprintf(outf, ", exp1=");
      dumpexpr(sp->exp1);
      }
    if (sp->exp2) {
      fprintf(outf, ", exp2=");
      dumpexpr(sp->exp2);
      }
    if (sp->exp3) {
      fprintf(outf, ", exp3=");
      dumpexpr(sp->exp3);
      }

    if (indent < 0)
      break;
    fprintf(outf, "\n");
    if (sp->stm1) {
      fprintf(outf, "%*sstm1=", indent, "");
      innerdumpstmt(sp->stm1, indent+5);
      }
    if (sp->stm2) {
      fprintf(outf, "%*sstm2=", indent, "");
      innerdumpstmt(sp->stm2, indent+5);
      }
    sp = sp->next;
    if (sp) {
      if (indent > 5)
        fprintf(outf, "%*s", indent-5, "");
      fprintf(outf, "next=");
      }
    }
#endif
}
//}}}
//{{{
void dumpstmt (sp, indent)
Stmt *sp;
int indent;
{
  fprintf(outf, "%*s", indent, "");
  innerdumpstmt(sp, indent);
}
//}}}
//{{{
void dumpstmt_file (f, sp)
FILE *f;
Stmt *sp;
{
  FILE *save = outf;
  Stmt *savenext = NULL;
  outf = f;
  if (sp) {
    savenext = sp->next;
    sp->next = NULL;
    }
  dumpstmt(sp, 5);
  if (sp)
    sp->next = savenext;
  outf = save;
}
//}}}
//{{{
void wrapup() {

  int i;
  for (i = 0; i < SYMHASHSIZE; i++)
    dumpsymtable(symtab[i]);
  }
//}}}

//{{{
void mem_summary() {

  printf("Summary of memory allocated but not freed:\n");
  printf("Total bytes = %d of %d\n", final_bytes, total_bytes);
  printf("Expressions = %d of %d\n", final_exprs, total_exprs);
  printf("Meanings =    %d of %d (%ld of %ld)\n",
         final_meanings, total_meanings, final_meanings / sizeof(Meaning), total_meanings / sizeof(Meaning));
  printf("Strings =     %d of %d\n", final_strings, total_strings);
  printf("Symbols =     %d of %d\n", final_symbols, total_symbols);
  printf("Types =       %d of %d (%ld of %ld)\n", final_types, total_types,
         final_types / sizeof(Type), total_types / sizeof(Type));
  printf("Statements =  %d of %d (%ld of %ld)\n", final_stmts, total_stmts,
         final_stmts / sizeof(Stmt), total_stmts / sizeof(Stmt));
  printf("Strlists =    %d of %d\n", final_strlists, total_strlists);
  printf("Literals =    %d of %d\n", final_literals, total_literals);
  printf("Ctxstacks =   %d of %d\n", final_ctxstacks, total_ctxstacks);
  printf("Temp vars =   %d of %d\n", final_tempvars, total_tempvars);
  printf("Input recs =  %d of %d\n", final_inprecs, total_inprecs);
  printf("Parens =      %d of %d\n", final_parens, total_parens);
  printf("Ptr Descs =   %d of %d\n", final_ptrdescs, total_ptrdescs);
  printf("Other =       %d of %d\n", final_misc, total_misc);
  printf("\n");
  }
//}}}
anyptr memlist;
//{{{
anyptr test_malloc(size, total, final)
int size, *total, *final;
  {
  anyptr p;

  p = malloc(size + 3*sizeof(long));

  ((anyptr *)p)[0] = memlist;
  memlist = p;
  ((long *)p)[1] = size;
  ((int **)p)[2] = final;
  total_bytes += size;
  final_bytes += size;
  *total += size;
  *final += size;

  return (anyptr)((long *)p + 3);
  }
//}}}
//{{{
void test_free(p)
anyptr p;
  {
  final_bytes -= ((long *)p)[1-3];
  *((int **)p)[2-3] -= ((long *)p)[1-3];
  ((long *)p)[1-3] *= -1;
  }
//}}}
//{{{
anyptr test_realloc(p, size)
anyptr p;
int size;
  {
  anyptr p2;
  p2 = test_malloc (size, &total_misc, &final_misc);
  memcpy (p2, p, size);
  test_free (p);
  return p2;
  }
//}}}

//{{{
int main (int argc, char** argv) {

  int numsearch;
  char* searchlist[50];
  char codefnbuf[200];
  char hdrfnbuf[200];
  Symbol* sp;
  Strlist* sl;
  int nobuffer = 0;
  int savequiet;

  init_stuff();

  int i = 0;
  while (i < argc && strcmp(argv[i], "-i")) i++;
  if (i < argc) {
    //{{{  simply show p2crc file
    showinitfile();
    return EXIT_SUCCESS;
    }
    //}}}

  initrc();
  setup_dir();

  // skip past switches to infname text
  char infnbuf[200];
  infname = infnbuf;
  *infname = 0;
  i = 0;
  while (i < argc && argv[i][0] == '-') i++;
  if (i >= argc)
    // use first non '-' argv as infname, pascal file to be translated
    strcpy (infname, argv[i]);

  // read p2crc
  i = 0;
  while (i < argc && strcmp (argv[i], "-c")) i++;
  if (i < argc-1) {
    //{{{  overide p2crc with -c option
    if (strcmp (argv[i+1], "-"))
      readrc (argv[i+1], 1);
    }
    //}}}
  else
    // use base .p2crc file
    readrc ("p2crc", 1);

  codefname = codefnbuf;
  *codefname = 0;

  hdrfname = hdrfnbuf;
  *hdrfname = 0;

  requested_module = NULL;
  found_module = 0;
  error_crash = 0;

  #ifdef CONSERVE_MEMORY
    conserve_mem = CONSERVE_MEMORY;
  #else
    conserve_mem = 1;
  #endif

  regression = 0;
  verbose = 0;
  partialdump = 1;
  numsearch = 0;
  argc--, argv++;
  while (argc > 0) {
    if (**argv == '-' && (*argv)[1]) {
      // options
      if (!strcmp(*argv, "-a")) {
        //{{{  -a ansiC
        ansiC = 1;
        }
        //}}}
      else if (argv[0][1] == 'L') {
        //{{{  -L language
        if (strlen(*argv) == 2 && argc > 1) {
          strcpy(language, ++*argv);
          --argc;
          }
        else
          strcpy(language, *argv + 2);

        upc (language);
        }
        //}}}
      else if (!strcmp(*argv, "-q")) {
        //{{{  -q quiet
        quietmode = 1;
        }
        //}}}
      else if (!strcmp(*argv, "-comp")) {
        //{{{  -comp compiler like usage
        /* Set defaults for compiler-like usage */
        maxalts = 100;
        elimdeadcode = 0;
        analyzeflow = 0;
        foldconsts = 1;
        foldstrconsts = 1;
        offsetforloops = 0;
        keepnulls = 1;
        hasstaticlinks = 1;
        mod_po2 = 0;
        div_po2 = 0;
        assumebits = 0;
        assumesigns = 0;
        formatstrings = 1;
        structfilesflag = 1;
        fullstrwrite = 1;
        }
        //}}}
      else if (!strcmp(*argv, "-check")) {
        //{{{  -check enable all errors
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
        //}}}
      else if (!strcmp(*argv, "-o")) {
        //{{{  -o c filename
        if (*codefname || --argc <= 0)
          usage();

        strcpy (codefname, *++argv);
        }
        //}}}
      else if (!strcmp(*argv, "-h")) {
         //{{{  -h header
         if (*hdrfname || --argc <= 0)
           usage();
         strcpy(hdrfname, *++argv);
          }
         //}}}
      else if (!strcmp(*argv, "-s")) {
        //{{{  s
        if (--argc <= 0)
          usage();
        char* cp = *++argv;

        if (!strcmp (cp, "-"))
          librfiles = NULL;
        else
          searchlist[numsearch++] = cp;
        }
        //}}}
      else if (!strcmp(*argv, "-c")) {
        //{{{  -c
        if (--argc <= 0)
           usage();
         argv++;
         /* already done above */
         }
        //}}}
      else if (!strcmp(*argv, "-v")) {
         //{{{  -v
         /* already done above */
         }
         //}}}
      else if (!strcmp(*argv, "-H")) {
        //{{{  -H help
         argc--, argv++;
        /* already done above */
        }
        //}}}
      else if (argv[0][1] == 'I') {
        //{{{  -I importdirs
        if (strlen(*argv) == 2 && argc > 1) {
          strlist_append (&importdirs, ++*argv);
          --argc;
         }
        else
          strlist_append (&importdirs, *argv + 2);
        }
        //}}}
      else if (argv[0][1] == 'p') {
        //{{{  -p show progress
        if (strlen(*argv) == 2)
          showprogress = 25;
        else
          showprogress = atoi (*argv + 2);
        nobuffer = 1;
        }
        //}}}
      else if (!strcmp (*argv, "-e")) {
        //{{{  -e copysource
        copysource++;
        }
        //}}}
      else if (!strcmp (*argv, "-t")) {
       //{{{  -t tokentrace
       tokentrace++;
       }
       //}}}
       else if (!strcmp (*argv, "-x")) {
        //{{{  -x error crash
        error_crash++;
        }
        //}}}
      else if (argv[0][1] == 'E') {
        //{{{  -E maxerrors
        if (strlen (*argv) == 2)
          maxerrors = 0;
        else
          maxerrors = atoi (*argv + 2);
        }
        //}}}
      else if (!strcmp (*argv, "-F")) {
        //{{{  F partial dump
        partialdump = 0;
        }
        //}}}
      else if (argv[0][1] == 'd') {
       //{{{  -d debug n
        nobuffer = 1;
        if (strlen (*argv) == 2)
          debug = 1;
        else
          debug = atoi (*argv + 2);
       }
       //}}}
      else if (argv[0][1] == 'B') {
        //{{{  -B
        if (strlen (*argv) == 2)
          i = 1;
        else
          i = atoi(*argv + 2);
          if (argc == 2 &&
            strlen (argv[1]) > 2 &&
            !strcmp (argv[1] + strlen(argv[1]) - 2, ".c")) {
            testlinebreaker (i, argv[1]);
            return EXIT_SUCCESS;
            }
          else
            testlinebreaker (i, NULL);
        }
        //}}}
      else if (argv[0][1] == 'C') {
        //{{{  -C
        if (strlen (*argv) == 2)
          cmtdebug = 1;
        else
          cmtdebug = atoi (*argv + 2);
        }
        //}}}
      else if (argv[0][1] == 'F') {
        //{{{  -F
        if (strlen (*argv) == 2)
          flowdebug = 1;
        else
          flowdebug = atoi (*argv + 2);
        }
        //}}}
      else if (!strcmp (*argv, "-R")) {
        //{{{  -R
        regression = 1;
        }
        //}}}
      else if (argv[0][1] == 'V') {
        //{{{  -V
        if (strlen (*argv) == 2)
          verbose = 1;
        else
          verbose = atoi (*argv + 2);
        }
        //}}}
      else if (argv[0][1] == 'M') {
        //{{{  -M
        if (strlen (*argv) == 2)
          conserve_mem = 1;
        else
          conserve_mem = atoi (*argv + 2);
        }
        //}}}
      else
        usage();
      }
    else if (!*infname)
      strcpy (infname, *argv);
    else if (!requested_module)
      requested_module = stralloc (*argv);
    else
      usage();

    argc--, argv++;
    }

  if (requested_module && !*codefname)
    strcpy (codefname, format_ss (modulefnfmt, infname, requested_module));

  if (*infname && strcmp (infname, "-")) {
    //{{{  check for and open infname file for read
    if (strlen (infname) > 2 &&
        !strcmp (infname + strlen(infname) - 2, ".c")) {
      fprintf(stderr, "What is wrong with this picture?\n");
      exit_failure();
      }

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
    setbuf (codef, NULL);      /* for debugging */
  outf = codef;
  outf_lnum = 1;

  logfile = NULL;
  if (verbose)
    openlogfile();

  setup_complete = 0;
  init_lex();
  leadingcomments();

  postrc();
  setup_comment();  /* must call this first */
  setup_lex();      /* must call this second */
  setup_out();
  setup_decl();     /* must call *after* setup_lex() */
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
      strlist_append (&sp->symbolnames, "===")->value = (long)findsymbol((char *)sl->value);
      }
    else
      strlist_append (&sp->symbolnames, "===")->value = 0;
    }

  strlist_empty (&synonyms);
  for (sl = addmacros; sl; sl = sl->next) {
    defmacro (sl->s, sl->value, "<macro>", 0);
    }

  strlist_empty (&addmacros);
  handle_nameof();
  setup_complete = 1;
  savequiet = quietmode;
  quietmode = 1;

  for (sl = librfiles; sl; sl = sl->next) {
    if (strlist_find (librfiles, sl->s) == sl)
      (void)p_search (format_none(sl->s), "pas", 0);
    }

  for (i = 0; i < numsearch; i++)
   (void)p_search (format_none (searchlist[i]), "pas", 1);

  quietmode = savequiet;
  p_program();
  end_source();
  flushcomments (NULL, -1, -1);
  showendnotes();

  check_unused_macros();
  if (!quietmode)
    printf ("\n");

  if (!showprogress && !quietmode)
    fprintf (stderr, "\n");

  output ("\n");
  if (requested_module && !found_module)
    error (format_s ("Module \"%s\" not found in file", requested_module));
  if (codef != stdout) {
    if (slashslash)
      output ("\n\n// End.\n");
    else
      output ("\n\n/* End. */\n");
    }

  // shutdown
  if (inf != stdin)
    fclose (inf);
  if (codef != stdout)
    fclose (codef);

  closelogfile();

  if (!quietmode) {
    //mem_summary();
    fprintf(stderr, "Translation completed.\n");
    }

  return EXIT_SUCCESS;
  }
//}}}
