// stuff.c
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
#define PROTO_STUFF_C
#include "main.h"

//{{{
/* Add a string to end of strlist */
Strlist* strlist_append (Strlist** base, char* s) {

  Strlist* p;

  while (*base)
    base = &(*base)->next;
  *base = p = ALLOCV(sizeof(Strlist) + strlen(s), Strlist, strlists);

  p->next = NULL;
  p->value = 0;
  strcpy(p->s, s);

  return p;
  }
//}}}
//{{{
/* Insert a string at front of strlist */
Strlist* strlist_insert (Strlist** base, char* s) {

  Strlist *p;

  p = ALLOCV(sizeof(Strlist) + strlen(s), Strlist, strlists);
  p->next = *base;
  *base = p;
  p->value = 0;
  strcpy(p->s, s);

  return p;
  }
//}}}
//{{{
/* Add a string to a sorted strlist */
Strlist* strlist_add (Strlist** base, char* s) {

  Strlist *p;

  while ((p = *base) && strcmp(p->s, s) < 0)
    base = &p->next;
  if (!p || strcmp(p->s, s)) {
    p = ALLOCV(sizeof(Strlist) + strlen(s), Strlist, strlists);
    p->next = *base;
    *base = p;
    strcpy(p->s, s);
    }

  p->value = 0;
  return p;
  }
//}}}

//{{{
/* Append two strlists together */
void strlist_mix (Strlist** base, Strlist* sl) {

  if (sl) {
    while (*base)
      base = &(*base)->next;
    *base = sl;
    }
  }
//}}}
//{{{
/* Remove the first element of a strlist */
void strlist_eat (Strlist** base) {

  Strlist *p;
  if ((p = *base) != NULL) {
    *base = p->next;
    FREE(p);
    }
  }
//}}}
//{{{
/* Remove all elements of a strlist */
void strlist_empty (Strlist** base) {

  Strlist *p;

  if (!base) {
    intwarning("strlist_empty", "NULL base pointer [312]");
    return;
    }
  while ((p = *base) != NULL) {
    *base = p->next;
    FREE(p);
    }
  }
//}}}
//{{{
/* Remove first occurrence of a given string */
void strlist_remove (Strlist** base, char* s) {

  Strlist *p;

  while ((p = *base) != NULL) {
    if (!strcmp(p->s, s)) {
      *base = p->next;
      FREE(p);
      }
    else
      base = &p->next;
    }
  }
//}}}
//{{{
/* Remove a given entry from a strlist */
void strlist_delete (Strlist** base, Strlist* sl) {

  Strlist *p;
  while ((p = *base) && p != sl)
    base = &p->next;
  if (p) {
    *base = p->next;
    FREE(p);
    }
  }
//}}}

//{{{
/* Find the first occurrence of a string */
Strlist* strlist_find (Strlist* base, char* s) {

  if (!s)
    return NULL;
  while (base && strcmp(base->s, s))
    base = base->next;
  return base;
  }
//}}}
//{{{
/* Case-insensitive version of strlist_find */
Strlist* strlist_cifind (Strlist* base, char* s) {

  if (!s)
    return NULL;
  while (base && strcicmp(base->s, s))
    base = base->next;
  return base;
  }
//}}}

//{{{
/* String comparisons */
int strcincmp (char* s1, char* s2, int n) {

  unsigned char ch1, ch2;

  while (--n >= 0) {
    if (!(ch1 = *s1++))
      return (*s2) ? -1 : 0;
    if (!(ch2 = *s2++))
      return 1;
    if (islower(ch1))
      ch1 = _toupper(ch1);
    if (islower(ch2))
      ch2 = _toupper(ch2);
    if (ch1 != ch2)
      return ch1 - ch2;
    }

  return 0;
  }
//}}}
//{{{
int strcicmp (char* s1, char* s2) {

  unsigned char ch1, ch2;

  for (;;) {
    if (!(ch1 = *s1++))
      return (*s2) ? -1 : 0;
    if (!(ch2 = *s2++))
      return 1;
    if (islower(ch1))
      ch1 = _toupper(ch1);
    if (islower(ch2))
      ch2 = _toupper(ch2);
    if (ch1 != ch2)
      return ch1 - ch2;
    }
  }
//}}}
//{{{
/* File name munching */
void fixfname (char* fn, char* ext) {

  char *cp, *cp2;

  if (!ext)
      return;
  cp = my_strrchr(fn, '.');
  cp2 = my_strrchr(fn, '/');
  if (cp && (!cp2 || cp > cp2)) {
    #if 0
      if (!cp[1])     /* remove trailing '.' */
        *cp = 0;
    #endif
    }
  else {
    strcat(fn, ".");
    strcat(fn, ext);
    }
  }
//}}}
//{{{
void removesuffix (char* fn) {

  char *cp;
  cp = my_strrchr(fn, '.');
  if (!cp)
    return;
  *cp = 0;
  }
//}}}
//{{{
/* Dynamically-allocated strings */
char* stralloc (char* s) {

  char *buf = ALLOC(strlen(s) + 1, char, strings);
  strcpy(buf, s);
  return buf;
  }
//}}}
//{{{
void strchange (char** v, char* s) {

  s = stralloc(s);   /* do this first in case **v and *s overlap */
  FREE(*v);
  *v = s;
  }
//}}}

// Handy string formatting
#define NUMBUF 8
static char *(formatbuf[NUMBUF]);
static int nextformat = -1;
#define getformat()  ((nextformat=(nextformat+1)%NUMBUF), formatbuf[nextformat])
//{{{  case defines
#define FF_UCASE   0x1
#define FF_LCASE   0x2
#define FF_REMSUFF 0x4
#define FF_UNDER   0x8     /* Thanks to William Bader for suggesting these */
#define FF_PRESERVE 0x10
#define FF_REMSLASH 0x20
#define FF_REMUNDER 0x40
//}}}
//{{{
Static void cvcase (char *buf, int flags) {

  char *cp, *cp2;
  int ulflag, i;

  if (flags & FF_PRESERVE) {
    ulflag = 0;
    for (cp = buf; *cp; cp++) {
      if (isupper(*cp))
        ulflag |= 1;
      else if (islower(*cp))
        ulflag |= 2;
      }

    if (ulflag == 3)
      flags &= ~(FF_UCASE | FF_LCASE);
    }
  if ((flags & FF_UNDER) && *buf) {
    for (cp = buf + 1; *cp; cp++) {
      if (isupper (*cp) && islower (cp[-1])) {
        for (i = (int)strlen (cp); i >= 0; i--)
          cp[i+1] = cp[i];
        *cp++ = '_';
        }
      }
    }

  if (flags & FF_UCASE) {
    if (flags & FF_LCASE) {
      for (cp = buf; *cp; cp++) {
        if (cp == buf || !isalpha (cp[-1]))
          *cp = toupper (*cp);
        else
          *cp = tolower (*cp);
        }
      } else
        upc(buf);
    }
  else if (flags & FF_LCASE)
    lwc (buf);

  if (flags & FF_REMUNDER) {
    for (cp = cp2 = buf; *cp; cp++) {
      if (isalnum (*cp))
        *cp2++ = *cp;
      }
    if (cp2 > buf)
      *cp2 = 0;
    }
  }
//}}}

//{{{
char* format_gen (char *fmt, long i1, long i2, double dbl, char *s1, char *s2, char *s3) {

  char *buf = getformat();
  char *dst = buf, *src = fmt, *cp, *cp2, *saves2 = s2;
  int wid, prec;
  int flags;
  char fmtbuf[50], *fp;
  Strlist *sl;

  while (*src) {
    if (*src != '%') {
      *dst++ = *src++;
      continue;
      }

    src++;
    wid = -1;
    prec = -1;
    flags = 0;
    fp = fmtbuf;
    *fp++ = '%';
    for (;;) {
      if (*src == '-' || *src == '+' || *src == ' ' || *src == '#')
        *fp++ = *src;
      else if (*src == '^')
        flags |= FF_UCASE;
      else if (*src == '_')
        flags |= FF_LCASE;
      else if (*src == 'R')
        flags |= FF_REMSUFF;
      else if (*src == '~')
        flags |= FF_UNDER;
      else if (*src == '!')
        flags |= FF_REMUNDER;
      else if (*src == '?')
        flags |= FF_PRESERVE;
      else if (*src == '/')
        flags |= FF_REMSLASH;
      else
        break;
      src++;
      }

    if (isdigit(*src)) {
      wid = 0;
      while (isdigit(*src))
      wid = wid*10 + (*fp++ = *src++) - '0';
      }
    else if (*src == '*') {
      //{{{  *
      src++;
      wid = i1;
      sprintf(fp, "%d", wid);
      fp = fp + strlen(fp);
      if (wid < 0)
      wid = -wid;
      i1 = i2;
      }
      //}}}

    if (*src == '.') {
      //{{{  dot
      if (*++src == '*') {
        prec = i1;
        i1 = i2;
        src++;
        }
      else {
        prec = 0;
        while (isdigit(*src))
          prec = prec*10 + (*src++) - '0';
        }
      sprintf(fp, ".%d", prec);
      fp = fp + strlen(fp);
      }
      //}}}

    if (*src == 'l' || *src == 'h')
      src++;

    switch (*src) {
      //{{{
      case '%':
        *dst++ = '%';
        break;
      //}}}
      case 'd':
      case 'i':
      case 'o':
      case 'u':
      case 'x':
      //{{{
      case 'X':
        *fp++ = 'l';
        *fp++ = *src;
        *fp = 0;
        sprintf(dst, fmtbuf, i1);
        i1 = i2;
        cvcase(dst, flags);
        dst = dst + strlen(dst);
        break;
      //}}}
      //{{{
      case 'c':
        *fp++ = *src;
        *fp = 0;
        sprintf(dst, fmtbuf, (int)i1);
        i1 = i2;
        cvcase(dst, flags);
        dst = dst + strlen(dst);
        break;
      //}}}
      case 'e':
      case 'E':
      case 'f':
      case 'g':
      //{{{
      case 'G':
        *fp++ = *src;
        *fp++ = 0;
        sprintf(dst, fmtbuf, dbl);
        cvcase(dst, flags);
        dst = dst + strlen(dst);
        break;
      //}}}
      case 's':
      //{{{
      case 'S':
        *fp++ = 's';
        *fp = 0;
        if (*src == 'S' && saves2) {
          cp = saves2;
          }
        else {
          cp = s1;
          s1 = s2;
          s2 = s3;
          }
        if (flags & FF_REMSUFF) {
          cp = format_s("%s", cp);
          removesuffix(cp);
          }

        if (!strcincmp(cp, "$$SHELL(", 8)) {
          cp += 8;
          cp2 = dst;
          while (*cp && *cp != ')')
            *cp2++ = *cp++;
          if (*cp)
            cp++;
          *cp2 = 0;
          sl = strlist_cifind(shellvars, dst);
          if (sl) {
            strcpy(dst, (char *)sl->value);
            dst += strlen(dst);
            }
          else
            warning(format_s("No such shell variable $$SHELL(%s) [341]", dst));
          }

        if (flags & FF_REMSLASH) {
          cp2 = cp + strlen(cp);
          while (cp2 >= cp && *cp2 != '/' && *cp2 != '\\' && *cp2 != ']' && *cp2 != ':')
            cp2--;
          if (cp2[1])
            cp = cp2 + 1;
          }
        sprintf(dst, fmtbuf, cp);
        cvcase(dst, flags);
        dst = dst + strlen(dst);
        break;
      //}}}
      }
    src++;
    }

  *dst = 0;
  return buf;
  }
//}}}
//{{{
char* format_none (fmt)
char *fmt;
{
    return format_gen(fmt, 0L, 0L, 0.0, NULL, NULL, NULL);
}
//}}}
//{{{
char* format_d (fmt, a1)
char *fmt;
long a1;
{
    return format_gen(fmt, a1, 0L, (double)a1, NULL, NULL, NULL);
}
//}}}
//{{{
char* format_g (fmt, a1)
char *fmt;
double a1;
{
    return format_gen(fmt, (long)a1, 0L, a1, NULL, NULL, NULL);
}
//}}}
//{{{
char* format_s (fmt, a1)
char *fmt, *a1;
{
    return format_gen(fmt, 0L, 0L, 0.0, a1, NULL, NULL);
}
//}}}
//{{{
char* format_ss (fmt, a1, a2)
char *fmt, *a1, *a2;
{
    return format_gen(fmt, 0L, 0L, 0.0, a1, a2, NULL);
}
//}}}
//{{{
char* format_sd (fmt, a1, a2)
char *fmt, *a1;
long a2;
{
    return format_gen(fmt, a2, 0L, (double)a2, a1, NULL, NULL);
}
//}}}
//{{{
char* format_ds (fmt, a1, a2)
char *fmt, *a2;
long a1;
{
    return format_gen(fmt, a1, 0L, (double)a1, a2, NULL, NULL);
}
//}}}
//{{{
char* format_dd (fmt, a1, a2)
char *fmt;
long a1, a2;
{
    return format_gen(fmt, a1, a2, (double)a1, NULL, NULL, NULL);
}
//}}}
//{{{
char* format_sss (fmt, a1, a2, a3)
char *fmt, *a1, *a2, *a3;
{
    return format_gen(fmt, 0L, 0L, 0.0, a1, a2, a3);
}
//}}}
//{{{
char* format_ssd (fmt, a1, a2, a3)
char *fmt, *a1, *a2;
long a3;
{
    return format_gen(fmt, a3, 0L, (double)a3, a1, a2, NULL);
}
//}}}
//{{{
char* format_sds (fmt, a1, a2, a3)
char *fmt, *a1, *a3;
long a2;
{
    return format_gen(fmt, a2, 0L, (double)a2, a1, a3, NULL);
}
//}}}

/* String conversions */
//{{{
int my_toupper (int c) {

  if (islower(c))
    return _toupper(c);
  else
    return c;
  }
//}}}
//{{{
int my_tolower (int c) {

  if (isupper(c))
    return _tolower(c);
  else
    return c;
  }
//}}}
//{{{
void upc (char* s) {

  for (; *s; s++)
    *s = toupper(*s);
  }
//}}}
//{{{
void lwc (char* s) {

  for (; *s; s++)
    *s = tolower(*s);
  }
//}}}
//{{{
char* strupper (char* s) {

  char *dest = getformat();
  char *s2 = dest;
  while (*s)
    *s2++ = toupper(*s++);
  *s2 = 0;
  return dest;
  }
//}}}
//{{{
char* strlower (char* s) {

  char *dest = getformat();
  char *s2 = dest;
  while (*s)
    *s2++ = tolower(*s++);
  *s2 = 0;
  return dest;
  }
//}}}
//{{{
char* my_strchr (char* cp, int c) {

  while (*cp && *cp != c)
    cp++;
  if (*cp)
    return cp;
  else
    return NULL;
  }
//}}}
//{{{
char* my_strrchr (char* cp, int c) {

  char *cp2 = NULL;
  while (*cp) {
  if (*cp == c)
    cp2 = cp;
    cp++;
    }

  return cp2;
  }
//}}}

Static char* tok_ptr;
//{{{
char* my_strtok (char* cp, char* delim) {

  if (cp)
    tok_ptr = cp;

  while (*tok_ptr && my_strchr(delim, *tok_ptr))
    tok_ptr++;
  if (!*tok_ptr)
    return NULL;

  cp = tok_ptr;
  while (*tok_ptr && !my_strchr(delim, *tok_ptr))
    tok_ptr++;
  if (*tok_ptr)
    *tok_ptr++ = 0;

  return cp;
  }
//}}}
//{{{
char* my_strtokq (char* cp, char* delim) {

  if (cp)
    tok_ptr = cp;

  while (*tok_ptr && my_strchr (delim, *tok_ptr))
    tok_ptr++;
  if (!*tok_ptr)
    return NULL;

  cp = tok_ptr;
  if (*cp == '"') {
    char *cp2;
    cp2 = cp = ++tok_ptr;
    while (*tok_ptr && *tok_ptr != '"') {
      if (*tok_ptr == '\\') {
        *cp2++ = *++tok_ptr;
        if (*tok_ptr)
          tok_ptr++;
        }
      else
        *cp2++ = *tok_ptr++;
      }

    if (*tok_ptr)
      *tok_ptr++ = 0;
    }
  else {
    if (*cp == '#')
      return NULL;
    while (*tok_ptr && !my_strchr (delim, *tok_ptr))
      tok_ptr++;
    if (*tok_ptr)
      *tok_ptr++ = 0;
    }

  return cp;
  }
//}}}
//{{{
long my_strtol (char* buf, char** ret, int base) {

  unsigned long val = 0;
  int dig, sign = 1;

  while (isspace (*buf))
    buf++;
  if (*buf == '+')
    buf++;
  else if (*buf == '-') {
    sign = -1;
    buf++;
    }

  if (*buf == '0') {
    if ((buf[1] == 'x' || buf[1] == 'X') && (base == 0 || base == 16)) {
      buf++;
      base = 16;
      }
    else if (base == 0)
      base = 8;
    buf++;
    }

  for (;;) {
    if (isdigit (*buf))
      dig = *buf - '0';
    else if (*buf >= 'a')
      dig = *buf - 'a' + 10;
    else if (*buf >= 'A')
      dig = *buf - 'A' + 10;
    else
      break;
    if (dig >= base)
      break;
    val = val * base + dig;
    buf++;
    }

  if (ret)
    *ret = buf;
  if (sign > 0)
    return val;
  else
    return -(signed)val;
  }
//}}}

//{{{
void init_stuff() {

  for (int i = 0; i < NUMBUF; i++)
    formatbuf[i] = ALLOC (1000, char, misc);
  }
//}}}
