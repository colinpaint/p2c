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
#define define_parameters
#define PROTO_DIR_C
#include "main.h"

//{{{
/* This function is called once when p2c is starting up, before
   the p2crc file has been read.
*/
void init_dir()
{
}
//}}}
//{{{
/* This function is called once when p2c is starting up, after
   the p2crc file has been read.
*/

void setup_dir()
{


}
//}}}

#define _setup(a,b)
//{{{
void setup_module (char *name, int defn) {

  if (!strcicmp(name, "SYSTEM"))
    decl_builtins();
  }
//}}}

void fix_parameters() {}
//{{{
Stmt* fix_statement (Stmt *sp) {

  return sp;
  }
//}}}
//{{{
Expr* fix_expression (Expr *ex, int env) {

  return ex;
  }
//}}}
//{{{
Expr* fix_bicall (Expr *ex, int env) {

  return NULL;
  }
//}}}

//{{{
// This function returns nonzero if the built-in function "name"
//  should be written "if (f(x))" rather than "if (f(x) != 0)" when used as a boolean
//  The call does *not* necessarily have //   to return a 1-or-0 value.
int boolean_bicall (char *name) {

  return (!strcmp(name, "strcmp") ||
          !strcmp(name, "strncmp") ||
          !strcmp(name, "memcmp") ||
          !strcmp(name, "feof") ||
          !strcmp(name, "feoln"));
  }
//}}}
//{{{
// The function "name" promises not to change certain of its
//  VAR-style parameters.  For each of arguments i = 0 through 15,
//  if bit 1<<i of the return value of this function is set, and
//  the i'th parameter is a pointer to an object, then the function
//  guarantees not to change that object.
unsigned int safemask_bicall (char *name) {

  Symbol*sp = findsymbol_opt(name);
  if (sp) {
    if (sp->flags & (STRUCTF|STRLAPF))
      return ~1;
    if (sp->flags & (NOSIDEEFF|DETERMF))
      return ~0;
    }

  if (!strcmp (name, "fwrite") || !strcmp(name, "memchr"))
    return 1;
  if (!strcmp (name, "memcpy") || !strcmp(name, "memmove"))
    return 2;
  if (!strcmp (name, "memcmp")) 
    return 3;
  if (!strcmp (name, "sprintf") || !strcmp(name, "fprintf"))
    return ~1;
  if (!strcmp (name, "printf"))
    return ~0;

  return 0;
  }
//}}}
//{{{
// The function "name" has side effects that could affect other variables
//   in the program besides those that are explicitly mentioned.
int sideeffects_bicall (char *name) {

  return 0;
  }
//}}}
