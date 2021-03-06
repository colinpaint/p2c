// parse.c
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
#define PROTO_PARSE_C
#include "main.h"

Static int silenceEcho = 1;
Static int trycount;
Static Strlist* includedfiles;
Static char echo_first;
Static int echo_pos;
//{{{
void setup_parse() {

  trycount = 0;
  includedfiles = NULL;
  echo_first = 1;
  echo_pos = 0;
  fixexpr_tryblock = 0;
  }
//}}}
//{{{
void echobreak() {

  if (echo_pos > 0) {
    printf ("\n");
    echo_pos = 0;
    echo_first = 0;
    }
  }
//}}}
//{{{
void echoword (char* name, int comma) {

  if (quietmode || showprogress || silenceEcho)
    return;

  FILE* f = (outf == stdout) ? stderr : stdout;

  if (!echo_first) {
    if (comma) {
      fprintf (f, ",");
      echo_pos++;
      }

    if (echo_pos + strlen (name) > 77) {
      fprintf (f, "\n");
      echo_pos = 0;
      }
    else {
      fprintf (f, " ");
      echo_pos++;
      }
    }

  echo_first = 0;
  fprintf (f, "%s", name);
  echo_pos += (int)strlen (name);
  fflush (f);
  }
//}}}
//{{{
void echoprocname (Meaning *mp) {
  echoword (mp->name, 1);
  }
//}}}

//{{{
Static void forward_decl (Meaning* func, int isextern) {

  int flags = 0;

  if (func->wasdeclared)
    return;

  if (isextern && func->constdefn && !checkvarmac (func))
    return;

  if (isextern) {
    output ("extern ");
    }
  else if (func->ctx->kind == MK_FUNCTION) {
    if (useAnyptrMacros)
      output ("Local ");
    else
      output ("static ");
    }
  else if ((use_static != 0 && !useAnyptrMacros) || (findsymbol (func->name)->flags & NEEDSTATIC)) {
    output ("static ");
    }
  else if (use_static && useAnyptrMacros) {
    output ("Static ");
    }

  if (func->type->basetype != tp_void || ansiC != 0) {
    if (func->type->smin)
      output (func->type->smin->val.s);
    else
      outbasetype (func->type, ODECL_FORWARD);
    flags = ODECL_SPACE;
    }

  outdeclarator (func->type, func->name, ODECL_FORWARD | flags);
  output (";\n");
  func->wasdeclared = 1;
  }
//}}}
//{{{
void need_forward_decl (Meaning* func) {
// Check if calling a parent procedure, whose body must  be declared forward

  if (func->wasdeclared)
    return;

  Meaning* mp;
  for (mp = curctx->ctx; mp; mp = mp->ctx) {
    if (mp == func) {
      if (func->ctx->kind == MK_FUNCTION)
        func->isforward = 1;
      else
        forward_decl(func, 0);
      return;
      }
    }
  }
//}}}
//{{{  statement utils
//{{{
void free_stmt (Stmt* sp) {

  if (sp) {
    free_stmt (sp->stm1);
    free_stmt (sp->stm2);
    free_stmt (sp->next);
    freeexpr (sp->exp1);
    freeexpr (sp->exp2);
    freeexpr (sp->exp3);
    FREE(sp);
    }
  }
//}}}

//{{{
Stmt* makestmt (enum stmtkind kind) {

  Stmt* sp = ALLOC(1, Stmt, stmts);

  sp->kind = kind;
  sp->next = NULL;
  sp->stm1 = NULL;
  sp->stm2 = NULL;
  sp->exp1 = NULL;
  sp->exp2 = NULL;
  sp->exp3 = NULL;
  sp->serial = curserial = ++serialcount;
  sp->trueprops = sp->falseprops = 0;
  sp->quietelim = 0;
  sp->doinit = 0;

  return sp;
  }
//}}}
//{{{
Stmt* makestmt_call (Expr* call) {

  Stmt* sp = makestmt(SK_ASSIGN);
  sp->exp1 = call;
  return sp;
  }
//}}}
//{{{
Stmt* makestmt_assign (Expr* lhs, Expr* rhs) {

  Stmt* sp = makestmt(SK_ASSIGN);
  sp->exp1 = makeexpr_assign(lhs, rhs);
  return sp;
  }
//}}}
//{{{
Stmt* makestmt_if (Expr* cond, Stmt* thn, Stmt* els) {

  Stmt* sp = makestmt (SK_IF);
  sp->exp1 = cond;
  sp->stm1 = thn;
  sp->stm2 = els;
  sp->quietelim = 1;

  return sp;
  }
//}}}
//{{{
Stmt* makestmt_seq (Stmt* s1, Stmt* s2) {

  if (!s1)
    return s2;

  if (!s2)
    return s1;

  Stmt* s1a;
  for (s1a = s1; s1a->next; s1a = s1a->next) ;
  s1a->next = s2;

  return s1;
  }
//}}}

//{{{
Stmt* copystmt (Stmt* sp) {


  if (sp) {
    Stmt* sp2 = makestmt(sp->kind);
    sp2->stm1 = copystmt(sp->stm1);
    sp2->stm2 = copystmt(sp->stm2);
    sp2->exp1 = copyexpr(sp->exp1);
    sp2->exp2 = copyexpr(sp->exp2);
    sp2->exp3 = copyexpr(sp->exp3);
    return sp2;
    }
  else
    return NULL;
  }
//}}}
//{{{
void nukestmt (Stmt* sp) {

  if (sp) {
    sp->kind = SK_ASSIGN;
    sp->exp1 = makeexpr_long(0);
    }
  }
//}}}

//{{{
void splicestmt (Stmt* sp, Stmt* spnew) {

  if (spnew) {
    Stmt* snext = sp->next;
    *sp = *spnew;
    while (sp->next)
      sp = sp->next;
    sp->next = snext;
    }

  else
    nukestmt (sp);
  }
//}}}

//{{{
int stmtcount (Stmt* sp) {

  int i = 0;
  while (sp) {
    i += 1 + stmtcount (sp->stm1) + stmtcount (sp->stm2);
    sp = sp->next;
    }

  return i;
  }
//}}}
//}}}

//{{{
Stmt* close_files_to_ctx (Meaning* ctx) {

  Stmt* splist = NULL;

  Meaning* ctx2 = curctx;
  while (ctx2 && ctx2 != ctx && ctx2->kind == MK_FUNCTION) {
    Meaning *mp;
    for (mp = ctx2->cbase; mp; mp = mp->cnext) {
      if (mp->kind == MK_VAR && isfiletype(mp->type, -1) && !mp->istemporary) {
        var_reference(mp);
        Stmt* sp = makestmt_if (makeexpr_rel(EK_NE, filebasename (makeexpr_var(mp)), makeexpr_nil ()),
                                makestmt_call (makeexpr_bicall_1 ("fclose", tp_void, filebasename (makeexpr_var(mp)))),
                                NULL);
        splist = makestmt_seq (splist, sp);
        }
      }

    ctx2 = ctx2->ctx;
    }

  return splist;
  }
//}}}
//{{{
void withrecordtype (Type* tp, Expr* ex) {

  Type* tp2 = tp;

  do {
    if (withlevel >= MAXWITHS-1)
      error("Too many nested WITHs");
    withlevel++;
    tp2 = tp2->basetype;
    } while (tp2);

  tp2 = tp;
  int i = withlevel;
  do {
    i--;
    withlist[i] = tp2;
    withexprs[i] = ex;
    tp2 = tp2->basetype;
    } while (tp2);
  }
//}}}
//{{{
int simplewith (Expr *ex) {

  switch (ex->kind) {
    case EK_VAR:
    case EK_CONST:
      return 1;
    case EK_DOT:
      return simplewith(ex->args[0]);
     default:
      return 0;
    }
  }
//}}}
//{{{
int simplefor (Stmt *sp, Expr* ex) {
  return (exprspeed (sp->exp2) <= 3 &&
          !checkexprchanged (sp->stm1, sp->exp2) &&
          !exproccurs (sp->exp2, ex));
}
//}}}
//{{{
int tryfuncmacro (Expr** exp, Meaning* mp) {

  char* name;
  Strlist* lp;
  Expr* ex = *exp, *ex2;

  ex2 = (mp) ? mp->constdefn : NULL;
  if (!ex2) {
    if (ex->kind == EK_BICALL || ex->kind == EK_NAME)
      name = ex->val.s;
    else if (ex->kind == EK_FUNCTION)
      name = ((Meaning *)ex->val.i)->name;
    else
      return 0;
    lp = strlist_cifind(funcmacros, name);
    ex2 = (lp) ? (Expr *)lp->value : NULL;
    }

  if (ex2) {
    *exp = replacemacargs(copyexpr(ex2), ex);
    freeexpr(ex);
    return 1;
    }

  return 0;
  }
//}}}

Static int memberfuncwithlevel;
//{{{  statement defines
#define SF_FUNC    0x1
#define SF_SAVESER 0x2
#define SF_FIRST   0x4
#define SF_IF    0x8

//{{{
#define addstmt(kind) \
  *spp = sp = makestmt (kind),   \
  spp = &(sp->next)
//}}}
//{{{
#define newstmt(kind) \
  addstmt (kind),   \
  steal_comments (firstserial, sp->serial, sflags & SF_FIRST),   \
  sflags &= ~SF_FIRST
//}}}
//}}}
//{{{
Static Stmt* p_stmt (Stmt* slist, int sflags) {

  Stmt *sbase = NULL, **spp = &sbase, **spp2, **spp3, **savespp;
  Stmt *defsp, **defsphook;
  register Stmt *sp;
  Stmt *sp2;
  long li1, li2, firstserial = 0, saveserial = 0, saveserial2;
  int i, forfixed, offset, line1, line2, toobig, isunsafe;
  Token savetok;
  char *name = NULL;
  Expr *ep, *ep2, *ep3, *forstep, *range, *swexpr, *trueswexpr;
  Type *tp;
  Meaning *mp, *tvar, *mp2, *tempmark, *tiplabel;
  Symbol *sym = NULL;
  enum exprkind ekind;
  Stmt *(*prochandler)();
  Strlist *cmt;

  tempmark = markstmttemps();

again:
  while (findlabelsym()) {
    newstmt(SK_LABEL);
    sp->exp1 = makeexpr_name (format_s (name_LABEL, curtokmeaning->name), tp_integer);
    gettok();
    needToken (TOK_COLON);
    }

  tiplabel = NULL;
  if (curtok == TOK_IDENT && which_lang == LANG_TIP && peeknextchar() == ':') {
    distinctdef++;
    tiplabel = addmeaning (curtoksym, MK_LABEL);
    tiplabel->isreturn = 1;
    distinctdef--;
    gettok();
    needToken (TOK_COLON);
    }

  firstserial = curserial;
  checkkeyword (TOK_TRY);
  checkkeyword (TOK_INLINE);
  checkkeyword (TOK_LOOP);
  checkkeyword (TOK_RETURN);
  if (modula2) {
    if (sflags & SF_SAVESER)
      goto stmtSeq;
    }

  switch (curtok) {
    //{{{
    case TOK_BEGIN:
      stmtSeq:
      if (sflags & (SF_FUNC|SF_SAVESER)) {
        saveserial = curserial;
        cmt = grabcomment (CMT_ONBEGIN);
        if (sflags & SF_FUNC)
          cmt = fixbeginendcomment (cmt);
        strlist_mix (&curcomments, cmt);
        }

      i = sflags & SF_FIRST;
      do {
        if (modula2) {
          if (curtok == TOK_BEGIN || curtok == TOK_SEMI)
            gettok();
          checkkeyword (TOK_ELSIF);
          if (curtok == TOK_ELSE || curtok == TOK_ELSIF)
            break;
          } else
        gettok();
        *spp = p_stmt (sbase, i);
        i = 0;
        while (*spp)
          spp = &((*spp)->next);
        } while (curtok == TOK_SEMI);

      if (sflags & (SF_FUNC|SF_SAVESER)) {
        cmt = grabcomment (CMT_ONEND);
        changecomments (cmt, -1, -1, -1, saveserial);
        if (sflags & SF_FUNC)
          cmt = fixbeginendcomment (cmt);
        strlist_mix (&curcomments, cmt);
        if (sflags & SF_FUNC)
          changecomments (curcomments, -1, saveserial, -1, 10000);
        curserial = saveserial;
        }

      checkkeyword(TOK_ELSIF);
      if (modula2 && (sflags & SF_IF))
        break;
      cmt = curcomments;
      curcomments = NULL;
      if (!needToken (TOK_END))
        skippasttoken (TOK_END);

      if ((sflags & SF_IF) && curtok == TOK_ELSE)
        changecomments (curcomments, CMT_POST, saveserial, CMT_PREELSE, -1);

      strlist_mix (&cmt, curcomments);
      curcomments = cmt;
      break;
    //}}}
    //{{{
    case TOK_CASE:
      gettok();
      swexpr = trueswexpr = p_ord_expr();
      if (nosideeffects(swexpr, 1)) {
        tvar = NULL;
        }
      else {
        tvar = makestmttempvar(swexpr->val.type, name_TEMP);
        swexpr = makeexpr_var(tvar);
        }

      savespp = spp;
      newstmt (SK_CASE);
      saveserial2 = curserial;
      sp->exp1 = trueswexpr;
      spp2 = &sp->stm1;
      tp = swexpr->val.type;
      defsp = NULL;
      defsphook = &defsp;
      if (!needToken (TOK_OF)) {
        skippasttoken (TOK_END);
        break;
        }

      i = 1;
      while (curtok == TOK_VBAR)
        gettok();

      checkkeyword (TOK_OTHERWISE);

      while (curtok != TOK_END && curtok != TOK_OTHERWISE && curtok != TOK_ELSE) {
          spp3 = spp2;
          saveserial = curserial;
          *spp2 = sp = makestmt(SK_CASELABEL);
          steal_comments(saveserial, sp->serial, i);
          spp2 = &sp->next;
          range = NULL;
          toobig = 0;
          for (;;) {
            ep = gentle_cast(p_expr(tp), tp);
            if (curtok == TOK_DOTS) {
              li1 = ord_value(eval_expr(ep));
              gettok();
              ep2 = gentle_cast(p_expr(tp), tp);
              li2 = ord_value(eval_expr(ep2));
              range = makeexpr_or(range, makeexpr_range(copyexpr(swexpr), ep, ep2, 1));
              if (li2 - li1 >= caselimit)
                toobig = 1;
              if (!toobig) {
                for (;;) {
                  sp->exp1 = makeexpr_val(make_ord(tp, li1));
                  if (li1 >= li2) break;
                    li1++;
                  serialcount--;   /* make it reuse the count */
                  sp->stm1 = makestmt(SK_CASELABEL);
                  sp = sp->stm1;
                  }
                }
              }
            else {
              sp->exp1 = copyexpr(ep);
              range = makeexpr_or(range, makeexpr_rel(EK_EQ, copyexpr(swexpr), ep));
              }

            if (curtok == TOK_COMMA) {
              gettok();
              serialcount--; /* make it reuse the count */
              sp->stm1 = makestmt(SK_CASELABEL);
              sp = sp->stm1;
              }
            else
              break;
            }

          needToken (TOK_COLON);
          if (toobig) {
            free_stmt(*spp3);
            spp2 = spp3;
            *defsphook = makestmt_if(range, p_stmt(NULL, SF_SAVESER|SF_IF), NULL);
            if (defsphook != &defsp && elseif != 0)
              (*defsphook)->exp2 = makeexpr_long(1);
              defsphook = &((*defsphook)->stm2);
            }
          else {
            freeexpr(range);
            sp->stm1 = p_stmt(NULL, SF_SAVESER|SF_IF);
          }

        i = 0;
        checkkeyword (TOK_OTHERWISE);
        if (curtok != TOK_END && curtok != TOK_OTHERWISE && curtok != TOK_ELSE) {
          if (curtok == TOK_VBAR) {
            while (curtok == TOK_VBAR)
              gettok();
            }
          else
            needToken (TOK_SEMI);
          checkkeyword (TOK_OTHERWISE);
          }
        }

      if (defsp) {
        *spp2 = defsp;
        spp2 = defsphook;
        if (tvar) {
          sp = makestmt_assign(makeexpr_var(tvar), trueswexpr);
          sp->next = *savespp;
          *savespp = sp;
          sp->next->exp1 = swexpr;
          }
        }
      else {
        if (tvar) {
          canceltempvar(tvar);
          freeexpr(swexpr);
          }
        }

      if (curtok == TOK_OTHERWISE || curtok == TOK_ELSE) {
        gettok();
        while (curtok == TOK_SEMI || curtok == TOK_COLON)
          gettok();
          /* changecomments(curcomments, CMT_TRAIL, curserial, CMT_POST, -1);   */
          i = SF_FIRST;
          while (curtok != TOK_END) {
            *spp2 = p_stmt(NULL, i);
            while (*spp2)
              spp2 = &((*spp2)->next);
            i = 0;
            if (curtok != TOK_SEMI)
              break;
            gettok();
            }
          if (!wexpecttok(TOK_END))
           skiptotoken(TOK_END);
        }
      else if (casecheck == 1 || (casecheck == 2 && range_flag)) {
        *spp2 = makestmt(SK_CASECHECK);
        }

      curserial = saveserial2;
      strlist_mix(&curcomments, grabcomment(CMT_ONEND));
      gettok();
      break;
    //}}}
    //{{{
    case TOK_FOR:
      forfixed = fixedflag;
      gettok();

      newstmt (SK_FOR);
      i = 0;
      if (which_lang == LANG_TIP) {
        wexpecttok(TOK_IDENT);
        ep = makeexpr_long(0);
        name = stralloc(curtokcase);
        sym = curtoksym;
        gettok();
        }
      else
        ep = p_sexpr(tp_integer);

      if (curtok == TOK_IN) {
        gettok();
        ep3 = p_expr(NULL);
        ord_range_expr(ep3->val.type->indextype, &ep2, &sp->exp2);
        ep2 = copyexpr(ep2);
        sp->exp2 = copyexpr(sp->exp2);
        ep2->val.type = sp->exp2->val.type = ep3->val.type->indextype;
        }
      else {
        if (!needToken (TOK_ASSIGN)) {
          skippasttoken (TOK_DO);
          break;
          }

        ep2 = makeexpr_charcast(p_expr(ep->val.type));
        if (curtok != TOK_DOWNTO) {
          if (!wexpecttok(TOK_TO)) {
            skippasttoken(TOK_DO);
            break;
            }
          }

        savetok = curtok;
        gettok();
        sp->exp2 = makeexpr_charcast(p_expr(ep->val.type));
        ep3 = NULL;
        }

      if (which_lang == LANG_TIP) {
        freeexpr(ep);
        mp2 = sym->mbase;
        if (mp2 && mp2->ctx == curctx &&
          mp2->kind == MK_VAR && mp2->anyvarflag &&
          !mp2->isactive && mp2->type == ep2->val.type) {
          mp2->isactive = 1;
          }
        else {
          distinctdef++;
          strcpy(curtokcase, name);
          curtoksym = sym;
          mp2 = addmeaning(sym, MK_VAR);
          distinctdef--;
          mp2->type = ep2->val.type;
          }

        mp2->anyvarflag = 0;
        ep = makeexpr_var(mp2);
        FREE(name);
        }
      else
        mp2 = NULL;

      checkkeyword (TOK_BY);
      if (curtok == TOK_BY) {
        gettok();
        forstep = p_expr (tp_integer);
        i = possiblesigns (forstep);
        if ((i & 5) == 5) {
          if (expr_is_neg (forstep)) {
            ekind = EK_GE;
            note ("Assuming FOR loop step is negative [252]");
            }
          else {
            ekind = EK_LE;
            note ("Assuming FOR loop step is positive [252]");
            }
          }
        else {
          if (!(i & 1))
            ekind = EK_LE;
          else
            ekind = EK_GE;
          }
        }
      else {
        if (savetok == TOK_DOWNTO) {
          ekind = EK_GE;
          forstep = makeexpr_long (-1);
          }
        else {
          ekind = EK_LE;
          forstep = makeexpr_long (1);
          }
        }

      tvar = NULL;
      swexpr = NULL;
      if (ep->kind == EK_VAR) {
        tp = findbasetype(ep->val.type, ODECL_NOPRES);
        if ((tp == tp_char || tp == tp_schar || tp == tp_uchar ||
             tp == tp_abyte || tp == tp_sbyte || tp == tp_ubyte || tp == tp_boolean) &&
             ((checkconst(sp->exp2, 0) &&
             tp != tp_sbyte && tp != tp_schar) || checkconst(sp->exp2, -128) || (checkconst(sp->exp2, 127) &&
             tp != tp_ubyte && tp != tp_uchar) || checkconst(sp->exp2, 255) || (tp == tp_char &&
             (useAnyptrMacros == 1 || unsignedchar != 1) &&
             isliteralconst(sp->exp2, NULL) == 2 && sp->exp2->val.i >= 128))) {
          swexpr = ep;
          tvar = makestmttempvar (tp_sshort, name_TEMP);
          ep = makeexpr_var (tvar);
          }
        else if (((tp == tp_sshort && (checkconst(sp->exp2, -32768) || checkconst(sp->exp2, 32767))) ||
                  (tp == tp_ushort && (checkconst(sp->exp2, 0) || checkconst(sp->exp2, 65535))))) {
          swexpr = ep;
          tvar = makestmttempvar (tp_integer, name_TEMP);
          ep = makeexpr_var (tvar);
          }
        else if (tp == tp_integer &&
                 (checkconst(sp->exp2, LONG_MAX) ||
                 (sp->exp2->kind == EK_VAR && sp->exp2->val.i == (int64_t)mp_maxint))) {
          swexpr = ep;
          tvar = makestmttempvar (tp_unsigned, name_TEMP);
          ep = makeexpr_var (tvar);
          }
        }

      sp->exp3 = makeexpr_assign (copyexpr(ep), makeexpr_inc(copyexpr(ep), copyexpr(forstep)));
      needToken (TOK_DO);

      forfixed = (fixedflag != forfixed);
      mp = makestmttempvar (ep->val.type, name_FOR);
      sp->stm1 = p_stmt (NULL, SF_SAVESER);

      if (mp2) {
        mp2->anyvarflag = 1;
        mp2->isactive = 0;
        }

      if (ep3) {
        if (ep3->val.type->kind == TK_SMALLSET) {
          range = makeexpr_bin(EK_LSH, ep->val.type, makeexpr_longcast(makeexpr_long(1), 1), enum_to_int(copyexpr(ep)));
          ep3 = makeexpr_rel(EK_NE, makeexpr_bin(EK_BAND, tp_integer, range, ep3), makeexpr_long(0));
          }
        else {
          ep3 = makeexpr_bicall_2(setinname, tp_boolean, makeexpr_arglong(copyexpr(ep), 0), ep3);
          }
        sp->stm1 = makestmt_if(ep3, sp->stm1, NULL);
        }

      if (tvar) {
        if (checkexprchanged(sp->stm1, swexpr))
          note (format_s ("Rewritten FOR loop won't work if it meddles with %s [253]", ((Meaning *)swexpr->val.i)->name));
        sp->stm1 = makestmt_seq (makestmt_assign (swexpr, makeexpr_var(tvar)), sp->stm1);
        }
      else if (offsetforloops && ep->kind == EK_VAR) {
        offset = checkvaroffset(sp->stm1, (Meaning *)ep->val.i);
        if (offset != 0) {
          ep3 = makeexpr_inc(copyexpr (ep), makeexpr_long (-offset));
          replaceexpr (sp->stm1, ep, ep3);
          freeexpr (ep3);
          ep2 = makeexpr_plus(ep2, makeexpr_long(offset));
          sp->exp2 = makeexpr_inc (sp->exp2, makeexpr_long (offset));
          }
        }

      if (!exprsame(ep, ep2, 1))
        sp->exp1 = makeexpr_assign (copyexpr (ep), copyexpr (ep2));

      isunsafe = ((!nodependencies (ep2, 2) && !nosideeffects(sp->exp2, 1)) ||
                  (!nodependencies (sp->exp2, 2) && !nosideeffects(ep2, 1)));

      if (forfixed || (simplefor (sp, ep) && !isunsafe)) {
        canceltempvar(mp);
        sp->exp2 = makeexpr_rel (ekind, ep, sp->exp2);
        }
      else {
        ep3 = makeexpr_neg (copyexpr (forstep));
        if ((checkconst (forstep, 1) || checkconst (forstep, -1)) &&
            sp->exp2->kind == EK_PLUS &&
            exprsame (sp->exp2->args[sp->exp2->nargs-1], ep3, 2)) {
          sp->exp2 = makeexpr_inc (sp->exp2, forstep);
          }
        else {
          freeexpr (forstep);
          freeexpr (ep3);
          ep3 = makeexpr_long (0);
          }

        if (forevalorder && isunsafe) {
          if (exprdepends (sp->exp2, ep)) {
            tvar = makestmttempvar (mp->type, name_TEMP);
            sp->exp1 = makeexpr_comma (
                 makeexpr_comma (makeexpr_assign (makeexpr_var (tvar), copyexpr (ep2)),
                                 makeexpr_assign (makeexpr_var (mp), sp->exp2)),
                                 makeexpr_assign (copyexpr (ep), makeexpr_var (tvar)));
            }
          else
            sp->exp1 = makeexpr_comma (sp->exp1, makeexpr_assign (makeexpr_var(mp), sp->exp2));
          }
        else {
          if (isunsafe)
            note ("Evaluating FOR loop limit before initial value [315]");
            sp->exp1 = makeexpr_comma (makeexpr_assign (makeexpr_var (mp), sp->exp2), sp->exp1);
          }
        sp->exp2 = makeexpr_inc (makeexpr_var (mp), ep3);
        sp->exp2 = makeexpr_rel (ekind, ep, sp->exp2);
        }

      freeexpr (ep2);
      break;
    //}}}
    //{{{
    case TOK_GOTO:
      gettok();
      if (findlabelsym()) {
        if (curtokmeaning->ctx != curctx) {
          curtokmeaning->val.i = 1;
          *spp = close_files_to_ctx(curtokmeaning->ctx);
          while (*spp)
            spp = &((*spp)->next);
          newstmt(SK_ASSIGN);
          var_reference(curtokmeaning->xnext);

          if (curtokmeaning->ctx->kind == MK_MODULE && !curtokmeaning->xnext->wasdeclared) {
            outsection(minorspace);
            declarevar(curtokmeaning->xnext, VDECL_ALL);
            curtokmeaning->xnext->wasdeclared = 1;
            outsection(minorspace);
            }

          sp->exp1 = makeexpr_bicall_2("longjmp", tp_void, makeexpr_var(curtokmeaning->xnext), makeexpr_long(1));
          }
        else {
          newstmt(SK_GOTO);
          sp->exp1 = makeexpr_name(format_s(name_LABEL, curtokmeaning->name), tp_integer);
          }
        }
      else {
        warning("Expected a label [263]");
        }
      gettok();
      break;
    //}}}
    //{{{
    case TOK_IF:
      gettok();
      newstmt(SK_IF);
      saveserial = curserial;
      curserial = ++serialcount;
      sp->exp1 = p_expr(tp_boolean);
      needToken (TOK_THEN);
      sp->stm1 = p_stmt(NULL, SF_SAVESER|SF_IF);

      changecomments(curcomments, -1, saveserial+1, -1, saveserial);
      checkkeyword(TOK_ELSIF);
      while (curtok == TOK_ELSIF) {
        gettok();
        sp->stm2 = makestmt(SK_IF);
        sp = sp->stm2;
        sp->exp1 = p_expr(tp_boolean);
        needToken(TOK_THEN);
        sp->stm1 = p_stmt(NULL, SF_SAVESER|SF_IF);
        sp->exp2 = makeexpr_long(1);
        }

      if (curtok == TOK_ELSE) {
        line1 = inf_lnum;
        strlist_mix (&curcomments, grabcomment(CMT_ONELSE));
        gettok();
        line2 = (curtok == TOK_IF) ? inf_lnum : -1;
        saveserial2 = curserial;
        sp->stm2 = p_stmt(NULL, SF_SAVESER|SF_IF);
        if (sp->stm2 && sp->stm2->kind == SK_IF && !sp->stm2->next && !modula2) {
          sp->stm2->exp2 = makeexpr_long(elseif > 0 || (elseif < 0 && line1 == line2));
          }
        saveserial = ++serialcount;
        changecomments(curcomments, -1, saveserial2, -1, saveserial);
        }

      if (modula2)
        needToken(TOK_END);

      curserial = saveserial;
      break;
    //}}}
    //{{{
    case TOK_INLINE:
      gettok();
      note ("Inline assembly language encountered [254]");

      if (curtok != TOK_LPAR) {   /* Macintosh style */
        newstmt (SK_ASSIGN);
        sp->exp1 = makeexpr_bicall_1 ("inline", tp_void, p_expr(tp_integer));
        while (curtok == TOK_COMMA) {
          gettok();
          newstmt (SK_ASSIGN);
          sp->exp1 = makeexpr_bicall_1 ("inline", tp_void, p_expr(tp_integer));
          }
        break;
        }

      do {
        name = getinlinepart();
        if (!*name)
          break;
        newstmt (SK_ASSIGN);
        sp->exp1 = makeexpr_bicall_1 ("asm", tp_void, makeexpr_string(format_s(" inline %s", name)));
        gettok();
        } while (curtok == TOK_SLASH);

      skipcloseparen();
      break;
    //}}}
    //{{{
    case TOK_LOOP:
      gettok();
      newstmt(SK_WHILE);
      sp->exp1 = makeexpr_long(1);
      sp->stm1 = p_stmt(NULL, SF_SAVESER);
      break;

    //}}}
    //{{{
    case TOK_REPEAT:
      newstmt (SK_REPEAT);
      saveserial = curserial;
      spp2 = &(sp->stm1);

      i = SF_FIRST;
      do {
        gettok();
        *spp2 = p_stmt (sp->stm1, i);
        i = 0;
        while (*spp2)
          spp2 = &((*spp2)->next);
        } while (curtok == TOK_SEMI);

      if (!needToken (TOK_UNTIL))
        skippasttoken (TOK_UNTIL);

      sp->exp1 = makeexpr_not (p_expr(tp_boolean));
      curserial = saveserial;
      strlist_mix (&curcomments, grabcomment(CMT_ONEND));

      break;
    //}}}
    //{{{
    case TOK_RETURN:
      gettok();

      newstmt (SK_RETURN);
      if (curctx->isfunction) {
        sp->exp1 = gentle_cast(p_expr(curctx->cbase->type), curctx->cbase->type);
        }
      break;
    //}}}
    //{{{
    case TOK_TRY:
      findsymbol("RECOVER")->flags &= ~KWPOSS;
      newstmt(SK_TRY);
      sp->exp1 = makeexpr_long(++trycount);
      spp2 = &(sp->stm1);

      i = SF_FIRST;
      do {
        gettok();
        *spp2 = p_stmt(sp->stm1, i);
        i = 0;
        while (*spp2)
          spp2 = &((*spp2)->next);
        } while (curtok == TOK_SEMI);

      if (!needToken(TOK_RECOVER))
        skippasttoken(TOK_RECOVER);

      sp->stm2 = p_stmt(NULL, SF_SAVESER);

      break;
    //}}}
    //{{{
    case TOK_WHILE:
      gettok();

      newstmt(SK_WHILE);
      sp->exp1 = p_expr(tp_boolean);
      needToken(TOK_DO);

      sp->stm1 = p_stmt(NULL, SF_SAVESER);

      break;
    //}}}
    //{{{
    case TOK_WITH:
      gettok();
      newstmt (SK_ASSIGN);
      sp->exp1 = makeexpr_long (0);

      ep = p_expr(NULL);
      if (ep->val.type->kind != TK_RECORD)
        warning ("Argument of WITH is not a RECORD [264]");

      i = withlevel;
      if (simplewith (ep)) {
        withrecordtype (ep->val.type, ep);
        mp = NULL;
        }
      else {
        /* need to save a temporary pointer */
        tp = makepointertype (ep->val.type);
        mp = makestmttempvar (tp, name_WITH);
        withrecordtype (ep->val.type, makeexpr_hat(makeexpr_var(mp), 0));
        }

      if (curtok == TOK_COMMA) {
        curtok = TOK_WITH;
        sp2 = p_stmt (NULL, (sflags & SF_FIRST) | SF_SAVESER);
        }
      else {
        needToken (TOK_DO);
        sp2 = p_stmt (NULL, (sflags & SF_FIRST) | SF_SAVESER);
        }

      withlevel = i;
      if (mp) {    /* if "with p^" for constant p, don't need temp ptr */
        if (ep->kind == EK_HAT && ep->args[0]->kind == EK_VAR &&
            !checkvarchanged (sp2, (Meaning *)ep->args[0]->val.i)) {
          replaceexpr (sp2, withexprs[withlevel]->args[0], ep->args[0]);
          freeexpr (ep);
          canceltempvar (mp);
          }
        else {
          freeexpr (sp->exp1);
          sp->exp1 = makeexpr_assign (makeexpr_var(mp), makeexpr_addr(ep));
          }
        }

       freeexpr (withexprs[withlevel]);

       *spp = sp2;
       while (*spp)
         spp = &((*spp)->next);

       break;
    //}}}
    //{{{
    case TOK_INCLUDE:
      badinclude();
      goto again;
    //}}}
    //{{{
    case TOK_ADDR:   /* flakey Turbo "@procptr := anyptr" assignment */
      newstmt(SK_ASSIGN);
      ep = p_expr(tp_void);
      if (needToken(TOK_ASSIGN))
        sp->exp1 = makeexpr_assign(ep, p_expr(ep->val.type));
      else
        sp->exp1 = ep;
      break;
    //}}}
    //{{{
    case TOK_INHERITED:
      newstmt (SK_ASSIGN);
      sp->exp1 = p_expr (tp_void);
      break;
    //}}}
    //{{{
    case TOK_IDENT:
      mp = curtokmeaning;
      if (mp == mp_str_hp)
        mp = curtokmeaning = mp_str_turbo;
      if (mp == mp_val_modula)
        mp = curtokmeaning = mp_val_turbo;
      if (mp == mp_blockread_ucsd)
        mp = curtokmeaning = mp_blockread_turbo;
      if (mp == mp_blockwrite_ucsd)
        mp = curtokmeaning = mp_blockwrite_turbo;
      if (mp == mp_dec_dec)
        mp = curtokmeaning = mp_dec_turbo;

      if (mp == mp_self_func) {
        gettok();
        needToken(TOK_DOT);
        if (!wexpecttok(TOK_IDENT))
          break;
        mp = curtokmeaning;
        }

      if (!mp) {
        sym = curtoksym;     /* make a guess at what the undefined name is... */
        name = stralloc(curtokcase);
        gettok();
        newstmt(SK_ASSIGN);
        if (curtok == TOK_ASSIGN) {
          gettok();
          ep = p_expr(NULL);
          mp = addmeaning(sym, MK_VAR);
          mp->name = name;
          mp->type = ep->val.type;
          sp->exp1 = makeexpr_assign(makeexpr_var(mp), ep);
          }
        else if (curtok == TOK_HAT || curtok == TOK_ADDR ||
                 curtok == TOK_LBR || curtok == TOK_DOT) {
          ep = makeexpr_name(name, tp_integer);
          ep = fake_dots_n_hats(ep);
          if (needToken(TOK_ASSIGN))
            sp->exp1 = makeexpr_assign(ep, p_expr(NULL));
          else
            sp->exp1 = ep;
           }
         else if (curtok == TOK_LPAR) {
           ep = makeexpr_bicall_0(name, tp_void);
           do {
             gettok();
             insertarg(&ep, ep->nargs, p_expr(NULL));
             } while (curtok == TOK_COMMA);
           skipcloseparen();
           sp->exp1 = ep;
           }
         else {
            sp->exp1 = makeexpr_bicall_0(name, tp_void);
            }

        if (!tryfuncmacro(&sp->exp1, NULL))
          undefsym(sym);
        }
      else if (mp->kind == MK_FUNCTION && !mp->isfunction) {
        mp->refcount++;
        if (curtokint >= 0) {
          ep = p_variable(tp_void);
          }
        else {
          gettok();
          ep = p_funccall(mp);
          }

        if (!mp->constdefn)
          need_forward_decl(mp);
        if (mp->handler && !(mp->sym->flags & LEAVEALONE) && !mp->constdefn) {
          prochandler = (Stmt *(*)())mp->handler;
          *spp = (*prochandler)(ep, slist);
          while (*spp)
             spp = &((*spp)->next);
          }
        else {
          newstmt(SK_ASSIGN);
          sp->exp1 = ep;
          }
        }
      else if (mp->kind == MK_SPECIAL) {
        gettok();
        if (mp->handler && !mp->isfunction) {
          if ((mp->sym->flags & LEAVEALONE) || mp->constdefn) {
            ep = makeexpr_bicall_0(mp->name, tp_void);
            if (curtok == TOK_LPAR) {
              do {
                gettok();
                insertarg(&ep, ep->nargs, p_expr(NULL));
                } while (curtok == TOK_COMMA);
              skipcloseparen();
              }
            newstmt(SK_ASSIGN);
            tryfuncmacro(&ep, mp);
            sp->exp1 = ep;
            }
          else {
            prochandler = (Stmt *(*)())mp->handler;
            *spp = (*prochandler)(mp, slist);
            while (*spp)
              spp = &((*spp)->next);
            }
          }
        else
          symclass(curtoksym);
        }
      else {
         newstmt(SK_ASSIGN);
         if (curtokmeaning->kind == MK_FUNCTION && peeknextchar() != '(') {
           Meaning *mp2 = curtokmeaning;
           if (mp2->xnext)
             mp2 = mp2->xnext;
           mp = curctx;
           while (mp && mp != mp2)
              mp = mp->ctx;
           if (mp)
              curtokmeaning = curtokmeaning->cbase;
           }
         ep = p_expr(tp_void);

         #if 0
           if (!(ep->kind == EK_SPCALL ||
                 (ep->kind == EK_COND &&
                  ep->args[1]->kind == EK_SPCALL)))
               wexpecttok(TOK_ASSIGN);
         #endif

         if (curtok == TOK_ASSIGN) {
           gettok();
           if (curtok == TOK_IDENT && !strcicmp(curtokbuf, "ZERO") && !curtokmeaning) {   /* VAX Pascal foolishness */
             gettok();
             ep2 = makeexpr_sizeof(copyexpr(ep), 0);
             sp->exp1 = makeexpr_bicall_3("memset", tp_void, makeexpr_addr(ep), makeexpr_long(0), ep2);
             }
           else
             sp->exp1 = makeexpr_assign(ep, p_expr(ep->val.type));
           }
         else
           sp->exp1 = ep;
         }
       break;
    //}}}
    //{{{
    default:
      break;    /* null statement */
    //}}}
    }
  freestmttemps (tempmark);

  if (sflags & SF_SAVESER)
    curserial = firstserial;

  if (tiplabel) {
    newstmt (SK_LABEL);
    sp->exp1 = makeexpr_name (format_s (name_LABEL, tiplabel->name), tp_integer);
    tiplabel->isactive = 0;
    }

  return sbase;
  }
//}}}

//{{{  brace defines
#define BR_NEVER        0x1     /* never use braces */
#define BR_FUNCTION     0x2     /* function body */
#define BR_THENPART     0x4     /* before an "else" */
#define BR_ALWAYS       0x8     /* always use braces */
#define BR_REPEAT       0x10    /* "do-while" loop */
#define BR_TRY          0x20    /* in a recover block */
#define BR_ELSEPART     0x40    /* after an "else" */
#define BR_CASE         0x80    /* case of a switch stmt */
//}}}
//{{{
Static int usebraces (sp, opts)
Stmt *sp;
int opts;
{
  if (opts & (BR_FUNCTION|BR_ALWAYS))
    return 1;
  if (opts & BR_NEVER)
     return 0;
  if (sp && sp->doinit)
    return 1;

  switch (bracesalways) {
    case 0:
      if (sp) {
        if (sp->next || sp->kind == SK_TRY ||
           (sp->kind == SK_IF && !sp->stm2) || (opts & BR_REPEAT))
          return 1;
        }
      break;

    case 1:
      return 1;

    default:
      if (sp) {
        if (sp->next || sp->kind == SK_IF || sp->kind == SK_WHILE ||
            sp->kind == SK_REPEAT || sp->kind == SK_TRY || sp->kind == SK_CASE || sp->kind == SK_FOR)
          return 1;
        }
      break;
    }

  if (sp != NULL && findcomment(curcomments, CMT_NOT | CMT_TRAIL, sp->serial) != NULL)
    return 1;
  return 0;
}
//}}}

#define outspnl(spflag) output((spflag) ? " " : "\n")
Meaning* outcontext;
//{{{
Static void outnl (int serial) {
  outtrailcomment (curcomments, serial, commentindent);
  }
//}}}
//{{{
Static void out_block (Stmt* spbase, int opts, int serial) {

  int i, j, braces, always, trynum, istrail, hascmt;
  int gotcomments = 0;
  int saveindent, saveindent2, delta, declspc;
  Stmt *sp = spbase;
  Stmt *sp2, *sp3;
  Meaning *ctx, *mp = NULL;
  Strlist *curcmt, *cmt, *savecurcmt = curcomments;
  Strlist *trailcmt, *begincmt, *endcmt;

  if (opts & BR_FUNCTION) {
    //{{{  function
    if (outcontext && outcontext->comments) {
      gotcomments = 1;
      curcomments = outcontext->comments;
      }
    attach_comments(spbase);
    }
    //}}}

  braces = usebraces(sp, opts);
  trailcmt = findcomment (curcomments, CMT_TRAIL, serial);
  begincmt = findcomment (curcomments, CMT_ONBEGIN, serial);
  istrail = 1;
  if (!trailcmt) {
    //{{{  not trailcnt
    trailcmt = begincmt;
    begincmt = NULL;
    istrail = 0;
    }
    //}}}

  endcmt = findcomment (curcomments, CMT_ONEND, serial);
  if ((begincmt || endcmt) && !(opts & BR_NEVER))
    braces = 1;

  if (opts & BR_ELSEPART) {
    //{{{  elsepart
    cmt = findcomment (curcomments, CMT_ONELSE, serial);
    if (cmt) {
      if (trailcmt) {
        out_spaces (bracecommentindent, commentoverindent,
        commentlen (cmt), 0);
        output("\001");
        outcomment (cmt);
        }
      else
        trailcmt = cmt;
      }
    }
    //}}}

  if (braces) {
    //{{{  braces
    j = (opts & BR_FUNCTION) ? funcopenindent : openbraceindent;
    if (!line_start()) {
      if (trailcmt && cur_column() + commentlen(trailcmt) + 2 > linewidth &&
          outindent + commentlen(trailcmt) + 2 < linewidth)  /*close enough*/
        i = 0;
      else if (opts & BR_ELSEPART)
        i = ((braceelseline & 2) == 0);
      else if (braceline >= 0)
        i = (braceline == 0);
      else
        i = ((opts & BR_FUNCTION) == 0);
      if (trailcmt && begincmt) {
        out_spaces (commentindent, commentoverindent, commentlen(trailcmt), j);
        outcomment (trailcmt);
        trailcmt = begincmt;
        begincmt = NULL;
        istrail = 0;
        }
      else
        outspnl(i);
      }

    if (line_start())
      singleindent(j);
    output("{");
    }
    //}}}
  else if (!sp) {
    if (!line_start())
      outspnl(!nullstmtline && !(opts & BR_TRY));
    if (line_start())
      singleindent (tabsize);
    output (";");
    }

  if (opts & BR_CASE)
    delta = 0;
  else {
    //{{{  not case
    delta = tabsize;
    if (opts & BR_FUNCTION)
      delta = adddeltas(delta, bodyindent);
    else if (braces)
      delta = adddeltas(delta, blockindent);
    }
    //}}}

  futureindent(delta);
  if (bracecombine && braces)
    i = applydelta (outindent, delta) - cur_column();
  else
    i = -1;

  if (commentvisible (trailcmt)) {
    if (line_start()) {
      singleindent (delta);
      out_spaces (commentoverindent, 1000, commentlen(trailcmt), 0);
      outcomment (trailcmt);
      }
    else /*if (commentlen(trailcmt) + cur_column() + 1 <= linewidth)*/ {
      out_spaces (istrail ? commentindent : bracecommentindent, commentoverindent, commentlen(trailcmt), delta);
      outcomment (trailcmt);
      }
    //else {
    //  output("\n");
    //  singleindent(delta);
    //  out_spaces(commentoverindent, 1000, commentlen(trailcmt), 0);
    //  outcomment(trailcmt);
    //  }
    i = -9999;
    }

  if (i > 0)
    out_spaces (i, 0, 0, 0);
  else if (i != -9999)
    output ("\n");

  saveindent = outindent;
  moreindent (delta);
  outcomment (begincmt);
  declspc = 0;

  while (sp) {
    if (declspc && !sp->doinit) {
      outsection (declspc);
      declspc = 0;
      }
    flushcomments (NULL, CMT_PRE, sp->serial);

    switch (sp->kind) {
      //{{{
      case SK_HEADER:
        ctx = (Meaning *)sp->exp1->val.i;
        eatblanklines();
        if (declarevars(ctx, 0) || (sp->next && sp->next->doinit)) {
          if (minorspace < 0)
            declspc = (cplus > 0) ? 0 : 1;
          else
            declspc = minorspace;
          if (sp->next && !sp->next->doinit) {
            outsection(declspc);
            declspc = 0;
            }
          }

        flushcomments(NULL, CMT_NOT | CMT_ONEND, serial);
        if (ctx->kind == MK_MODULE) {
          if (ctx->anyvarflag) {
            if (*name_MAIN) {
              outsection(declspc);
              declspc = 0;
              output(format_s(name_MAIN, ""));
              if (spacefuncs)
                output(" ");
              output("(argc,");
              if (spacecommas)
               output(" ");
              output("argv);\n");
              }
            }
          else {
            outsection (declspc);
            declspc = 0;
            output ("static int _was_initialized = 0;\n");
            output ("if (_was_initialized++)\n");
            singleindent (tabsize);
            output ("return;\n");
            }
          while (initialcalls) {
            outsection (declspc);
            declspc = 0;
            output (initialcalls->s);
            output (";\n");
            strlist_remove (&initialcalls, initialcalls->s);
            }
          }
        else {
          if (ctx->varstructflag && ctx->ctx->kind == MK_FUNCTION && ctx->ctx->varstructflag) {
            outsection(declspc);
            declspc = 0;
            output (format_s(name_VARS, ctx->name));
            output (".");
            output (format_s(name_LINK, ctx->ctx->name));
            output (" = ");
            output (format_s(name_LINK, ctx->ctx->name));
            output (";\n");
            }

          for (mp = ctx->cbase; mp; mp = mp->cnext) {
            if ((mp->kind == MK_VAR || mp->kind == MK_VARREF) &&
                ((mp->varstructflag &&      /* initializers which were moved */
                  mp->cnext &&              /* into a varstruct, so they */
                  mp->cnext->snext == mp && /* must be initialized now */
                  mp->cnext->constdefn && ctx->kind == MK_FUNCTION) ||
                 (mp->constdefn && mp->type->kind == TK_ARRAY &&
                  mp->constdefn->val.type->kind == TK_STRING && !initpacstrings))) {
              outsection (declspc);
              declspc = 0;
              if (mp->type->kind == TK_ARRAY) {
                output ("memcpy(");
                out_var (mp, 2);
                output (",\002");
                if (spacecommas)
                  output (" ");
                if (mp->constdefn) {
                  output (makeCstring (mp->constdefn->val.s, mp->constdefn->val.i));
                  mp->constdefn = NULL;
                  }
                else
                  out_var (mp->cnext, 2);
                output (",\002");
                if (spacecommas)
                  output (" ");
                output ("sizeof(");
                out_type (mp->type, 0);
                output ("))");
                }
              else {
                out_var (mp, 2);
                output (" = ");
                out_var (mp->cnext, 2);
                }
              output (";\n");
              }
            }
          }
        break;
      //}}}
      //{{{
      case SK_RETURN:
        output("return");
        if (sp->exp1) {
          switch (returnparens) {

            case 0:
             output(" ");
             out_expr(sp->exp1);
             break;

            case 1:
              if (spaceexprs != 0)
                output(" ");
              out_expr_parens(sp->exp1);
              break;

            default:
              if (sp->exp1->kind == EK_VAR ||
                  sp->exp1->kind == EK_CONST ||
                  sp->exp1->kind == EK_LONGCONST ||
                  sp->exp1->kind == EK_BICALL ||
                  sp->exp1->kind == EK_NAME) {
                output(" ");
                out_expr(sp->exp1);
                }
              else {
                if (spaceexprs != 0)
                  output(" ");
                out_expr_parens(sp->exp1);
                }
              break;
            }
          }

        output(";");
        outnl(sp->serial);
        break;

      //}}}
      //{{{
      case SK_ASSIGN:
        if (sp->doinit) {
          mp = (Meaning*)sp->exp1->args[0]->val.i;
          mp->constdefn = sp->exp1->args[1];
          declarevar (mp, VDECL_HEADER|VDECL_BODY);
          mp->constdefn = NULL;

          if (findcomment (curcomments, CMT_TRAIL, sp->serial)) {
            output (";");
            outnl (sp->serial);
            flushcomments (&mp->comments, -1, -1);
            }
          else
            declarevar (mp, VDECL_TRAILER);
          }
        else {
          out_expr_stmt (sp->exp1);
          output (";");
          outnl (sp->serial);
          }

        break;
      //}}}
      //{{{
      case SK_CASE:
        output ("switch (");
        out_expr (sp->exp1);
        output (")");
        outspnl (braceline <= 0);

        if (line_start())
          singleindent (openbraceindent);

        output ("{");
        outnl (sp->serial);
        saveindent2 = outindent;
        moreindent (tabsize);
        moreindent (switchindent);

        sp2 = sp->stm1;
        while (sp2 && sp2->kind == SK_CASELABEL) {
          outsection(casespacing);
          sp3 = sp2;
          i = 0;
          hascmt = (findcomment(curcomments, -1, sp2->serial) != NULL);
          singleindent(caseindent);
          flushcomments(NULL, CMT_PRE, sp2->serial);
          for (;;) {
            if (i)
              singleindent(caseindent);
            i = 0;

            output ("case ");
            out_expr (sp3->exp1);
            output (":\001");
            sp3 = sp3->stm1;
            if (!sp3 || sp3->kind != SK_CASELABEL)
              break;
            if (casetabs != 1000)
              out_spaces (casetabs, 0, 0, 0);
            else {
              output ("\n");
              i = 1;
              }
            }
          if (sp3)
            out_block (sp3, BR_NEVER|BR_CASE, sp2->serial);
          else {
            outnl (sp2->serial);
            if (!hascmt) {
              if (slashslash)
                output ("// blank case\n");
              else
                output ("/* blank case */\n");
              }
            }

          output ("break;\n");
          flushcomments (NULL, -1, sp2->serial);
          sp2 = sp2->next;
          }

        if (sp2) {
          outsection (casespacing);
          singleindent (caseindent);
          flushcomments (NULL, CMT_PRE, sp2->serial);
          output ("default:");
          out_block (sp2, BR_NEVER|BR_CASE, sp2->serial);
          output ("break;\n");
          flushcomments (NULL, -1, sp2->serial);
          }
        outindent = saveindent2;

        if (line_start())
          singleindent (closebraceindent);

        output ("}");
        curcmt = findcomment (curcomments, CMT_ONEND, sp->serial);
        if (curcmt)
          outcomment (curcmt);
        else
          output ("\n");

        break;
      //}}}
      //{{{
      case SK_CASECHECK:
        output(name_CASECHECK);
        output("();   /* CASE value range error */\n");
        break;
      //}}}
      //{{{
      case SK_FOR:
        if (sp->doinit) {
          mp = (Meaning *)sp->exp1->args[0]->val.i;
          flushcomments(&mp->comments, CMT_PRE, -1);
         }

       output("for (");

       if (for_allornone)
         output ("\007");
         if (sp->exp1 || sp->exp2 || sp->exp3 || spaceexprs > 0) {
           if (sp->doinit) {
             mp->constdefn = sp->exp1->args[1];
             declarevar (mp, VDECL_HEADER|VDECL_BODY);
             strlist_mix (&mp->comments, curcomments);
             curcomments = mp->comments;
             mp->comments = NULL;
             mp->constdefn = NULL;
             }
           else if (sp->exp1)
             out_expr_top (sp->exp1);
           else if (spaceexprs > 0)
             output(" ");

           output (";\002 ");
           if (sp->exp2)
             out_expr (sp->exp2);
           output (";\002 ");
           if (sp->exp3)
             out_expr_top (sp->exp3);
           }
         else
           output (";;");

        output (")");
        out_block (sp->stm1, 0, sp->serial);
        break;

      //}}}
      //{{{
      case SK_LABEL:
        if (!line_start())
          output ("\n");

        singleindent (labelindent);

        out_expr (sp->exp1);
        output (":");
        if (!sp->next)
          output (" ;");
        outnl (sp->serial);

        break;
      //}}}
      //{{{
      case SK_GOTO:
        /* what about non-local goto's? */
        output ("goto ");
        out_expr (sp->exp1);
        output (";");
        outnl (sp->serial);
        break;
      //}}}
      //{{{
      case SK_IF:
        sp2 = sp;

        for (;;) {
          output ("if (");
          out_expr_bool (sp2->exp1);
          output (")");

          if (sp2->stm2) {
            cmt = extractcomment(&curcomments, CMT_ONELSE, sp->serial+1);
            i = (!cmt && sp2->stm2->kind == SK_IF && !sp2->stm2->next && ((sp2->stm2->exp2)
                  ? checkconst(sp2->stm2->exp2, 1)
                  : (elseif > 0)));
            if (braceelse && (usebraces(sp2->stm1, 0) || usebraces(sp2->stm2, 0) || i))
              always = BR_ALWAYS;
            else
              always = 0;
            out_block(sp2->stm1, BR_THENPART|always, sp2->serial);

            changecomments (curcomments, -1, sp2->serial+1, CMT_PRE, sp2->stm2->serial);
            flushcomments(NULL, -1, sp2->serial);
            strlist_mix(&cmt, curcomments);
            curcomments = cmt;
            output ("else");
            sp2 = sp2->stm2;
            if (i) {
              output (" ");
               if (sp2->stm1) {
                 changecomments (curcomments, CMT_PRE, sp2->serial, CMT_PRE, sp2->stm1->serial);
                 changecomments (curcomments, CMT_POST, sp2->serial, CMT_PRE, sp2->stm1->serial);
                 }
              }
            else {
              out_block(sp2, BR_ELSEPART|always, sp2->serial+1);
              break;
              }
            }
          else {
            out_block(sp2->stm1, 0, sp2->serial);
            break;
            }
          }
        break;
      //}}}
      //{{{
      case SK_REPEAT:
        output ("do");
        out_block (sp->stm1, BR_ALWAYS|BR_REPEAT, sp->serial);
        output ("while (");
        out_expr_bool (sp->exp1);
        output (");");

        cmt = findcomment (curcomments, CMT_ONEND, sp->serial);
        if (commentvisible (cmt)) {
          out_spaces (commentindent, commentoverindent, commentlen(cmt), 0);
          output ("\001");
          outcomment (cmt);
          }
        else
          output ("\n");
       break;
      //}}}
      //{{{
      case SK_TRY:
        trynum = (int)sp->exp1->val.i;
        output (format_d ("TRY(try%d);", trynum));
        out_block (sp->stm1, BR_NEVER|BR_TRY, sp->serial);

        if (sp->exp2)
          output (format_ds ("RECOVER2(try%d,%s);", trynum, format_s (name_LABEL, format_d ("try%d", trynum))));
        else
          output (format_d ("RECOVER(try%d);", trynum));

        out_block (sp->stm2, BR_NEVER|BR_TRY, sp->serial);
        output (format_d ("ENDTRY(try%d);\n", trynum));
        break;
      //}}}
      //{{{
      case SK_WHILE:
        output ("while (");
        out_expr_bool (sp->exp1);
        output (")");
        out_block (sp->stm1, 0, sp->serial);
        break;
      //}}}
      //{{{
      case SK_BREAK:
        output ("break;");
        outnl (sp->serial);
        break;
      //}}}
      //{{{
      case SK_CONTINUE:
        output ("continue;");
        outnl (sp->serial);
        break;
      //}}}
      //{{{
      default:
        intwarning ("out_block",
        format_s ("Misplaced statement kind %s [265]",
        stmtkindname (sp->kind)));
        break;
      //}}}
      }

    flushcomments (NULL, -1, sp->serial);
    sp = sp->next;
    }

  if (opts & BR_FUNCTION) {
    //{{{  function
    cmt = extractcomment (&curcomments, CMT_ONEND, serial);
    if (findcomment (curcomments, -1, -1) != NULL)  /* check for non-DONE */
      output ("\n");

    flushcomments (NULL, -1, -1);
    curcomments = cmt;
    }
    //}}}
  outindent = saveindent;

  if (braces) {
    //{{{  braces
    if (line_start()) {
      if (opts & BR_FUNCTION)
        singleindent(funccloseindent);
      else
        singleindent(closebraceindent);
      }
    output("}");
    i = 1;
    cmt = findcomment(curcomments, CMT_ONEND, serial);
    if (!(opts & BR_REPEAT) && commentvisible(cmt)) {
      out_spaces(bracecommentindent, commentoverindent,
      commentlen(cmt), 0);
      output("\001");
      outcomment(cmt);
      i = 0;
      }

    if (i) {
      outspnl ((opts & BR_REPEAT) || ((opts & BR_THENPART) && (braceelseline & 1) == 0 &&
               !findcomment(curcomments, -1, serial)));
      }
    }
    //}}}
  if (gotcomments) {
    outcontext->comments = curcomments;
    curcomments = savecurcmt;
    }
  }
//}}}
//{{{
/* Should have a way to convert GOTO's to the end of the function to RETURN's */
/* Convert "_RETV = foo;" at end of function to "return foo" */
Static int checkreturns (Stmt** spp, int nearret) {

  Stmt *sp;
  Expr *rvar, *ex;
  Meaning *mp;
  int spnearret, spnextreturn;
  int result = 0;

  while ((sp = *spp)) {
    spnextreturn = (sp->next &&
                    sp->next->kind == SK_RETURN && sp->next->exp1 &&
                    isretvar(sp->next->exp1) == curctx->cbase);
    spnearret = (nearret && !sp->next) || spnextreturn;
    result = 0;
    switch (sp->kind) {
      //{{{
      case SK_ASSIGN:
        ex = sp->exp1;
        if (ex->kind == EK_ASSIGN || structuredfunc(ex)) {
          rvar = ex->args[0];
          mp = isretvar(rvar);
          if (mp == curctx->cbase && spnearret) {
            if (ex->kind == EK_ASSIGN) {
              if (mp->kind == MK_VARPARAM) {
                ex = makeexpr_comma(ex, makeexpr_var(mp));
                }
              else {
                ex = grabarg(ex, 1);
                mp->refcount--;
                }
              }

            sp->exp1 = ex;
            sp->kind = SK_RETURN;

            if (spnextreturn) {
              mp->refcount--;
              sp->next = sp->next->next;
              }

            result = 1;
            }
          }

        break;
      //}}}

      case SK_RETURN:
      case SK_GOTO:
        result = 1;
        break;

      case SK_IF:
        result = checkreturns(&sp->stm1, spnearret) &    /* NOT && */
                 checkreturns(&sp->stm2, spnearret);
        break;

      case SK_TRY:
        (void) checkreturns(&sp->stm1, 0);
        (void) checkreturns(&sp->stm2, spnearret);
        break;

      /* should handle CASE statements as well */
      default:
        (void) checkreturns(&sp->stm1, 0);
        (void) checkreturns(&sp->stm2, 0);
        break;
      }

    spp = &sp->next;
    }

  return result;
  }
//}}}
//{{{
/* Replace all occurrences of one expression with another expression */

Expr* replaceexprexpr (Expr* ex, Expr* oldex, Expr* newex, int keeptype) {

  int i;
  Type *type;

  for (i = 0; i < ex->nargs; i++)
    ex->args[i] = replaceexprexpr(ex->args[i], oldex, newex, keeptype);

  if (exprsame(ex, oldex, 2)) {
    if (ex->val.type->kind == TK_POINTER &&
        ex->val.type->basetype == oldex->val.type) {
      freeexpr(ex);
      return makeexpr_addr(copyexpr(newex));
      }
    else if (oldex->val.type->kind == TK_POINTER &&
             oldex->val.type->basetype == ex->val.type) {
      freeexpr(ex);
      return makeexpr_hat(copyexpr(newex), 0);
      }
    else {
      type = ex->val.type;
      freeexpr(ex);
      ex = copyexpr(newex);
      if (keeptype)
        ex->val.type = type;
      return ex;
      }
    }

  return resimplify(ex);
  }
//}}}
//{{{
void replaceexpr (Stmt *sp, Expr *oldex, Expr *newex) {

  while (sp) {
    replaceexpr (sp->stm1, oldex, newex);
    replaceexpr (sp->stm2, oldex, newex);

    if (sp->exp1)
      sp->exp1 = replaceexprexpr (sp->exp1, oldex, newex, 1);

    if (sp->exp2)
      sp->exp2 = replaceexprexpr (sp->exp2, oldex, newex, 1);

    if (sp->exp3)
      sp->exp3 = replaceexprexpr (sp->exp3, oldex, newex, 1);

    sp = sp->next;
    }
  }
//}}}
//{{{
Stmt* mixassignments (Stmt *sp, Meaning *mp) {

  if (!sp)
    return NULL;

  sp->next = mixassignments(sp->next, mp);
  if (sp->next &&
      sp->kind == SK_ASSIGN &&
      sp->exp1->kind == EK_ASSIGN &&
      sp->exp1->args[0]->kind == EK_VAR &&
      (!mp || mp == (Meaning *)sp->exp1->args[0]->val.i) &&
      ord_type(sp->exp1->args[0]->val.type)->kind == TK_INTEGER &&
      nodependencies(sp->exp1->args[1], 0) &&
      sp->next->kind == SK_ASSIGN &&
      sp->next->exp1->kind == EK_ASSIGN &&
      (exprsame(sp->exp1->args[0], sp->next->exp1->args[0], 1) || (mp && mp->istemporary)) &&
      exproccurs(sp->next->exp1->args[1], sp->exp1->args[0]) == 1) {
    sp->next->exp1->args[1] = replaceexprexpr (sp->next->exp1->args[1],
                                               sp->exp1->args[0],
                                               sp->exp1->args[1], 1);
    if (mp && mp->istemporary)
      canceltempvar (mp);

     return sp->next;
    }

  return sp;
  }
//}}}

Static Stmt bogusreturn = { SK_RETURN, NULL, NULL, NULL, NULL, NULL, NULL };
//{{{
Static int isescape (Expr* ex) {

  if (ex->kind == EK_BICALL && (!strcmp(ex->val.s, name_ESCAPE) ||
                                !strcmp(ex->val.s, name_ESCIO) ||
                                !strcmp(ex->val.s, name_ESCIO2) ||
                                !strcmp(ex->val.s, name_OUTMEM) ||
                                !strcmp(ex->val.s, name_CASECHECK) ||
                                !strcmp(ex->val.s, name_NILCHECK) ||
                                !strcmp(ex->val.s, "_exit") ||
                                !strcmp(ex->val.s, "exit")))
    return 1;

  if (ex->kind == EK_CAST)
    return isescape(ex->args[0]);

  return 0;
  }
//}}}
//{{{
Static int deadendblock (Stmt* sp, int breaks) {
/* check if a block can never exit by falling off the end */

  if (!sp)
    return 0;

  while (sp->next)
    sp = sp->next;

  return (sp->kind == SK_RETURN ||
          sp->kind == SK_CASECHECK ||
          ((sp->kind == SK_GOTO || sp->kind == SK_BREAK ||
          sp->kind == SK_CONTINUE) && breaks) ||
          (sp->kind == SK_IF && deadendblock(sp->stm1, breaks) && deadendblock(sp->stm2, breaks)) ||
          (sp->kind == SK_ASSIGN && isescape(sp->exp1)));
  }
//}}}
//{{{
int expr_is_bool (Expr* ex, int want) {

  long val;
  if (ex->val.type == tp_boolean && isconstexpr(ex, &val))
    return (val == want);

  return 0;
  }
//}}}
//{{{
int implies (Expr* c1, Expr* c2, int not1, int not2) {
/* Returns 1 if c1 implies c2, 0 otherwise */
/* If not1 is true, then checks if (!c1) implies c2; similarly for not2 */
/* Identities used:
        c1 -> (c2a && c2b)      <=>     (c1 -> c2a) && (c1 -> c2b)
        c1 -> (c2a || c2b)      <=>     (c1 -> c2a) || (c1 -> c2b)
        (c1a && c1b) -> c2      <=>     (c1a -> c2) || (c1b -> c2)
        (c1a || c1b) -> c2      <=>     (c1a -> c2) && (c1b -> c2)
        (!c1) -> (!c2)          <=>     c2 -> c1
        (a == b) -> c2(b)       <=>     c2(a)
        !(c1 && c2)             <=>     (!c1) || (!c2)
        !(c1 || c2)             <=>     (!c1) && (!c2)
*/
/* This could be smarter about, e.g., (a>5) -> (a>0) */

  Expr *ex;
  int i;

  if (c1->kind == EK_EQ && c1->args[0]->val.type == tp_boolean) {
    if (checkconst(c1->args[0], 1)) {     /* things like "flag = true" */
        return implies(c1->args[1], c2, not1, not2);
    } else if (checkconst(c1->args[1], 1)) {
        return implies(c1->args[0], c2, not1, not2);
    } else if (checkconst(c1->args[0], 0)) {
        return implies(c1->args[1], c2, !not1, not2);
    } else if (checkconst(c1->args[1], 0)) {
        return implies(c1->args[0], c2, !not1, not2);
    }
  }

  if (c2->kind == EK_EQ && c2->args[0]->val.type == tp_boolean) {
    if (checkconst(c2->args[0], 1)) {
        return implies(c1, c2->args[1], not1, not2);
    } else if (checkconst(c2->args[1], 1)) {
        return implies(c1, c2->args[0], not1, not2);
    } else if (checkconst(c2->args[0], 0)) {
        return implies(c1, c2->args[1], not1, !not2);
    } else if (checkconst(c2->args[1], 0)) {
        return implies(c1, c2->args[0], not1, !not2);
    }
  }
  switch (c2->kind) {
    //{{{
    case EK_AND:
        if (not2)               /* c1 -> (!c2a || !c2b) */
            return (implies(c1, c2->args[0], not1, 1) ||
                    implies(c1, c2->args[1], not1, 1));
        else                    /* c1 -> (c2a && c2b) */
            return (implies(c1, c2->args[0], not1, 0) &&
                    implies(c1, c2->args[1], not1, 0));
    //}}}
    //{{{
    case EK_OR:
        if (not2)               /* c1 -> (!c2a && !c2b) */
            return (implies(c1, c2->args[0], not1, 1) &&
                    implies(c1, c2->args[1], not1, 1));
        else                    /* c1 -> (c2a || c2b) */
            return (implies(c1, c2->args[0], not1, 0) ||
                    implies(c1, c2->args[1], not1, 0));
    //}}}
    //{{{
    case EK_NOT:                /* c1 -> (!c2) */
        return (implies(c1, c2->args[0], not1, !not2));

    //}}}
    //{{{
    case EK_CONST:
        if ((c2->val.i != 0) != not2)  /* c1 -> true */
            return 1;
        break;
    //}}}
    //{{{
    default:
        break;
    //}}}
    }

  switch (c1->kind) {
    //{{{
    case EK_AND:
        if (not1)               /* (!c1a || !c1b) -> c2 */
            return (implies(c1->args[0], c2, 1, not2) &&
                    implies(c1->args[1], c2, 1, not2));
        else                    /* (c1a && c1b) -> c2 */
            return (implies(c1->args[0], c2, 0, not2) ||
                    implies(c1->args[1], c2, 0, not2));
    //}}}
    //{{{
    case EK_OR:
        if (not1)               /* (!c1a && !c1b) -> c2 */
            return (implies(c1->args[0], c2, 1, not2) ||
                    implies(c1->args[1], c2, 1, not2));
        else                    /* (c1a || c1b) -> c2 */
            return (implies(c1->args[0], c2, 0, not2) &&
                    implies(c1->args[1], c2, 0, not2));
    //}}}
    //{{{
    case EK_NOT:                /* (!c1) -> c2 */
        return (implies(c1->args[0], c2, !not1, not2));
    //}}}
    //{{{
    case EK_CONST:
        if ((c1->val.i != 0) == not1)  /*  false -> c2 */
            return 1;
        break;
    //}}}
    case EK_EQ:                 /* (a=b) -> c2 */
    case EK_ASSIGN:             /* (a:=b) -> c2 */
    //{{{
    case EK_NE:                 /* (a<>b) -> c2 */
        if ((c1->kind == EK_NE) == not1) {
            if (c1->args[0]->kind == EK_VAR) {
                ex = replaceexprexpr(copyexpr(c2), c1->args[0], c1->args[1], 1);
                i = expr_is_bool(ex, !not2);
                freeexpr(ex);
                if (i)
                    return 1;
            }
            if (c1->args[1]->kind == EK_VAR) {
                ex = replaceexprexpr(copyexpr(c2), c1->args[1], c1->args[0], 1);
                i = expr_is_bool(ex, !not2);
                freeexpr(ex);
                if (i)
                    return 1;
            }
        }
        break;
    //}}}
    //{{{
    default:
        break;
    //}}}
    }

  if (not1 == not2 && exprequiv(c1, c2)) {    /* c1 -> c1 */
    return 1;
    }

  return 0;
  }
//}}}
//{{{
void infiniteloop (Stmt *sp) {

  switch (infloopstyle) {
    case 1:  /* write "for (;;) ..." */
      sp->kind = SK_FOR;
      freeexpr (sp->exp1);
      sp->exp1 = NULL;
      break;

    case 2:  /* write "while (1) ..." */
      sp->kind = SK_WHILE;
      freeexpr (sp->exp1);
      sp->exp1 = makeexpr_val(make_ord(tp_boolean, 1));
      break;

    case 3:  /* write "do ... while (1)" */
      sp->kind = SK_REPEAT;
      freeexpr (sp->exp1);
      sp->exp1 = makeexpr_val(make_ord(tp_boolean, 1));
      break;

    default: /* leave it alone */
      break;
    }
  }
//}}}

//{{{  print utils
//{{{
Expr* print_func (Expr *ex) {

  if (!ex || ex->kind != EK_BICALL)
    return NULL;

  if ((!strcmp(ex->val.s, "printf") && ex->args[0]->kind == EK_CONST) ||
      !strcmp(ex->val.s, "putchar") || !strcmp(ex->val.s, "puts"))
    return ex_output;

  if ((!strcmp(ex->val.s, "fprintf") || !strcmp(ex->val.s, "sprintf")) &&
      ex->args[1]->kind == EK_CONST)
    return ex->args[0];

  if (!strcmp(ex->val.s, "putc") || !strcmp(ex->val.s, "fputc") || !strcmp(ex->val.s, "fputs"))
    return ex->args[1];

  return NULL;
  }
//}}}
//{{{
int printnl_func (Expr *ex) {

  char *cp, ch;
  int i, len;

  if (!strcmp (ex->val.s, "printf") || !strcmp (ex->val.s, "puts") || !strcmp (ex->val.s, "fputs")) {
    if (ex->args[0]->kind != EK_CONST)
      return 0;
    cp = (char*)ex->args[0]->val.s;
    len = (int)ex->args[0]->val.i;
    }

  else if (!strcmp(ex->val.s, "fprintf")) {
    if (ex->args[1]->kind != EK_CONST)
      return 0;
    cp = (char*)ex->args[1]->val.s;
    len = (int)ex->args[1]->val.i;
    }

  else if (!strcmp (ex->val.s, "putchar") || !strcmp (ex->val.s, "putc") || !strcmp (ex->val.s, "fputc")) {
    if (ex->args[0]->kind != EK_CONST)
      return 0;
    ch = (char)ex->args[0]->val.i;
    cp = &ch;
    len = 1;
    }

  else
    return 0;

  for (i = 1; i <= len; i++)
    if (*cp++ != '\n')
      return 0;

  return len + (!strcmp (ex->val.s, "puts"));
  }
//}}}
//{{{
Expr* chg_printf (Expr *ex) {

  Expr *fex;

  if (!strcmp(ex->val.s, "putchar")) {
    ex = makeexpr_sprintfify (grabarg(ex, 0));
    canceltempvar (istempvar (ex->args[0]));
    strchange (&ex->val.s, "printf");
    delfreearg (&ex, 0);
    ex->val.type = tp_void;
    }
  else if (!strcmp(ex->val.s, "putc") ||
           !strcmp(ex->val.s, "fputc") ||
           !strcmp(ex->val.s, "fputs")) {
    fex = copyexpr(ex->args[1]);
    ex = makeexpr_sprintfify (grabarg(ex, 0));
    canceltempvar (istempvar(ex->args[0]));
    strchange (&ex->val.s, "fprintf");
    ex->args[0] = fex;
    ex->val.type = tp_void;
    }
  else if (!strcmp(ex->val.s, "puts")) {
    ex = makeexpr_concat (makeexpr_sprintfify (grabarg (ex, 0)), makeexpr_string ("\n"), 1);
    strchange (&ex->val.s, "printf");
    delfreearg (&ex, 0);
    ex->val.type = tp_void;
    }

  if (!strcmp (ex->val.s, "fprintf") && exprsame (ex->args[0], ex_output, 1)) {
    delfreearg (&ex, 0);
    strchange (&ex->val.s, "printf");
    }

  return ex;
  }
//}}}
//{{{
Expr* mix_printf (Expr *ex, Expr *ex2) {

  int i;

  ex = chg_printf (ex);
  ex2 = chg_printf (copyexpr (ex2));
  i = (!strcmp (ex->val.s, "printf")) ? 0 : 1;

  ex->args[i] = makeexpr_concat (ex->args[i], ex2->args[i], 0);

  for (i++; i < ex2->nargs; i++)
    insertarg (&ex, ex->nargs, ex2->args[i]);

  return ex;
  }
//}}}
//}}}
//{{{  Data flow analysis
#define MAX_PROP_CHECKS  32
//{{{  static vars
Static Expr* flowexprs[MAX_PROP_CHECKS];
Static Stmt* flowassigns[MAX_PROP_CHECKS];
Static int flowprops[MAX_PROP_CHECKS];    /* One of PROP_... */
Static int numpropchecks;
Static long globalprops, checkelimprops;
Static int flowbreaks, flowrecord;
Static int flowuseful;
//}}}
//{{{
Static int propcheckexprokay (ex)
Expr *ex;
{
    switch (ex->kind) {
  case EK_VAR:
      if (((Meaning *)ex->val.i)->isref)
    return 0;
      else if (((Meaning *)ex->val.i)->varstructflag)
    return 2;
      else if (nodependencies(ex, 2))
    return 1;
      else
    return 2;
  case EK_NAME:
      return 2;
  case EK_DOT:
      return propcheckexprokay(ex->args[0]);
  default:
      return 0;
    }
}
//}}}
//{{{
Static int findpropexpr (ex, prop, add)
Expr *ex;
int prop, add;
{
    int i, j;

    for (i = 0; i < numpropchecks; i++) {
  if (exprsame(ex, flowexprs[i], 2) && flowprops[i] == prop)
      return i;
    }
    if (add && (j = propcheckexprokay(ex)) != 0 &&
  numpropchecks < MAX_PROP_CHECKS) {
  flowexprs[i] = ex;
  flowprops[i] = prop;
  flowassigns[i] = NULL;
  if (j == 2)
      globalprops |= (1L << i);
  numpropchecks++;
  return i;
    }
    return -1;
}
//}}}
//{{{
Static void findpropchecksexpr (ex)
Expr *ex;
{
    int i;

    if (!ex || numpropchecks >= MAX_PROP_CHECKS)
  return;
    for (i = 0; i < ex->nargs; i++)
  findpropchecksexpr(ex->args[i]);
    switch (ex->kind) {

        case EK_EQ:
        case EK_NE:
      if (checkconst(ex->args[1], 0))
    findpropexpr(ex->args[0], PROP_ZERO, 1);
      if (checkconst(ex->args[0], 0))
    findpropexpr(ex->args[1], PROP_ZERO, 1);
      break;

  case EK_VAR:
  case EK_NAME:
  case EK_DOT:
      if (ex->val.type == tp_boolean ||
    (ex->val.type && ex->val.type->kind == TK_POINTER))
    findpropexpr(ex, PROP_ZERO, 1);
      break;

  default:
      break;
    }
}
//}}}
//{{{
Static void findpropchecks (sp)
Stmt *sp;
{
    while (sp) {
  findpropchecksexpr(sp->exp1);
  findpropchecksexpr(sp->exp2);
  findpropchecksexpr(sp->exp3);
  findpropchecks(sp->stm1);
  findpropchecks(sp->stm2);
  sp = sp->next;
    }
}
//}}}
//{{{
int proveexprprop (ex, sp, prop)   /* 0 = false, 1 = true, -1 = unknown */
Expr *ex;
Stmt *sp;
int prop;
{
    int i = findpropexpr(ex, prop, 0);
    if (i >= 0) {
  if (sp->trueprops & (1L << i))
      return 1;
  if (sp->falseprops & (1L << i))
      return 0;
  return -1;
    }
    switch (ex->kind) {

        case EK_CONST:
        case EK_LONGCONST:
      switch (prop) {
    case PROP_ZERO:
        return (checkconst(ex, 0));
    }
      break;

  case EK_CAST:
  case EK_ACTCAST:
  case EK_LITCAST:
      if (prop == PROP_ZERO)
    return proveexprprop(ex->args[0], sp, prop);
      break;

  case EK_ADDR:
      if (prop == PROP_ZERO)
    return 0;
      break;

  case EK_COND:
      i = proveexprprop(ex->args[1], sp, prop);
      if (i >= 0 && proveexprprop(ex->args[2], sp, prop) == i)
    return i;
      break;

  default:
      break;
    }
    return -1;
}
//}}}
//{{{
Expr* flow_fixexpr (ex, sp, env)
Expr *ex;
Stmt *sp;
int env;
{
    int i;

    switch (ex->kind) {

        case EK_VAR:
        case EK_HAT:
        case EK_NAME:
      if (env != ENV_LVALUE) {
    for (i = 0; i < numpropchecks; i++) {
        if (exprsame(ex, flowexprs[i], 2)) {
      switch (flowprops[i]) {
          case PROP_ZERO:
              if (sp->trueprops & (1L << i)) {
            return makeexpr_val(make_ord(ex->val.type,
                 0));
        } else if ((sp->falseprops & (1L << i)) &&
             ex->val.type == tp_boolean) {
            return makeexpr_val(make_ord(tp_boolean,
                 1));
        }
        break;
      }
        }
    }
      }
      break;

  case EK_ASSIGN:
      if (checkconst(ex->args[1], 0) &&
    proveexprprop(ex->args[0], sp, PROP_ZERO) == 1)
    ex = grabarg(ex, 1);
      else if (checkconst(ex->args[1], 1) &&
         ex->args[1]->val.type == tp_boolean &&
         proveexprprop(ex->args[0], sp, PROP_ZERO) == 0)
    ex = grabarg(ex, 1);
      break;

  default:
      break;
    }
    return ex;
}
//}}}
//{{{
Static void checkpropzero (ex, truep, falsep, nonzero)
Expr *ex;
long *truep, *falsep;
int nonzero;
{
    int i;

    switch (ex->kind) {

  case EK_AND:
      if (nonzero) {
    for (i = 0; i < ex->nargs; i++)
        checkpropzero(ex->args[i], truep, falsep, nonzero);
      }
      break;

  case EK_OR:
      if (!nonzero) {
    for (i = 0; i < ex->nargs; i++)
        checkpropzero(ex->args[i], truep, falsep, nonzero);
      }
      break;

  case EK_NOT:
      checkpropzero(ex->args[0], truep, falsep, !nonzero);
      break;

  case EK_EQ:
  case EK_NE:
      if (checkconst(ex->args[1], 0)) {
    checkpropzero(ex->args[0], truep, falsep,
            (ex->kind == EK_EQ) ? !nonzero : nonzero);
      } else if (ex->args[1]->kind == EK_CONST) {
    i = ((ex->kind == EK_EQ) ? nonzero : !nonzero);
    if (i || ex->args[1]->val.type == tp_boolean)
        checkpropzero(ex->args[0], truep, falsep, i);
      }
      break;

  default:
      i = findpropexpr(ex, PROP_ZERO, 0);
      if (i >= 0) {
    if (nonzero) {
        *truep &= ~(1L << i);
        *falsep |= (1L << i);
    } else {
        *truep |= (1L << i);
        *falsep &= ~(1L << i);
    }
      }
      break;

    }
}
//}}}
//{{{
Static void checkpropsexpr (ex, sp, truep, falsep)
Expr *ex;
Stmt *sp;
long *truep, *falsep;
{
    int i, j;
    Expr *ex1;
    Meaning *mp;
    long tp1, fp1;

    if (!ex)
  return;
    if (ex->kind != EK_ASSIGN && ex->kind != EK_AND &&
  ex->kind != EK_OR && ex->kind != EK_COND) {
  for (i = 0; i < ex->nargs; i++)
      checkpropsexpr(ex->args[i], sp, truep, falsep);
    }
    switch (ex->kind) {

  case EK_ASSIGN:
      checkpropsexpr(ex->args[1], sp, truep, falsep);
      ex1 = ex->args[0];
      while (ex1->kind == EK_DOT)
    ex1 = ex1->args[0];
      if (ex1->kind == EK_VAR || ex1->kind == EK_NAME) {
    for (i = 0; i < numpropchecks; i++) {
        if (exprsame(ex->args[0], flowexprs[i], 2)) {
      if ((flowassigns[i] || ex1->val.type == tp_boolean) &&
          !(globalprops & (1L << i)))
          checkelimprops |= (1L << i);
      flowassigns[i] = sp;
      j = proveexprprop(ex->args[1], sp, flowprops[i]);
      if (j == 1)
          *truep |= (1L << i);
      else
          *truep &= ~(1L << i);
      if (j == 0)
          *falsep |= (1L << i);
      else
          *falsep &= ~(1L << i);
        } else if (exprdepends(flowexprs[i], ex->args[0])) {
      *truep &= ~(1L << i);
      *falsep &= ~(1L << i);
        }
    }
      } else {
    checkpropsexpr(ex->args[0], sp, truep, falsep);
    *truep &= ~globalprops;
    *falsep &= ~globalprops;
      }
      break;

  case EK_POSTINC:
  case EK_POSTDEC:
      for (i = 0; i < numpropchecks; i++) {
    if (exprdepends(flowexprs[i], ex->args[0])) {
        *truep &= ~(1L << i);
        *falsep &= ~(1L << i);
    }
      }
      break;

  case EK_BICALL:
      if (!strcmp(ex->val.s, "assign")) {
        i = findpropexpr(ex->args[0], PROP_ZERO, 0);
        if (i >= 0) {
    *truep &= ~(1L << i);
    *falsep &= ~(1L << i);
        }
      }
      break;

  case EK_FUNCTION:
  case EK_SPCALL:
            if (ex->kind == EK_FUNCTION) {
                i = 0;
                mp = ((Meaning *)ex->val.i)->type->fbase;
            } else {
                i = 1;
                mp = ex->args[0]->val.type->basetype->fbase;
            }
            for ( ; mp && i < ex->nargs; i++) {
    if (mp->isref) {
        for (j = 0; j < numpropchecks; j++) {
      if (exprdepends(flowexprs[j], ex->args[i])) {
          *truep &= ~(1L << j);
          *falsep &= ~(1L << j);
      }
        }
    }
                if (mp->kind == MK_VARPARAM &&
        mp->type == tp_strptr && mp->anyvarflag)
                    i++;
                mp = mp->xnext;
            }
      *truep &= ~globalprops;
      *falsep &= ~globalprops;
      break;

  case EK_DIVIDE:
  case EK_DIV:
  case EK_MOD:
      i = findpropexpr(ex->args[1], PROP_ZERO, 0);
      if (i >= 0) {
    *truep &= ~(1L << i);
    *falsep |= (1L << i);
      }
      break;

  case EK_HAT:
      if (!nilcheck) {
    i = findpropexpr(ex->args[0], PROP_ZERO, 0);
    if (i >= 0) {
        *truep &= ~(1L << i);
        *falsep |= (1L << i);
    }
      }
      break;

  case EK_ADDR:
      for (i = 0; i < numpropchecks; i++) {
    if (exprdepends(flowexprs[i], ex->args[0]))
        flowprops[i] = PROP_INVALID;
      }
      break;

  case EK_AND:
  case EK_OR:
      checkpropsexpr(ex->args[0], sp, truep, falsep);
      for (i = 1; i < ex->nargs; i++) {
    tp1 = *truep;
    fp1 = *falsep;
    checkpropsexpr(ex->args[i], sp, &tp1, &fp1);
    *truep &= tp1;
    *falsep &= fp1;
      }
      break;

  case EK_COND:
      checkpropsexpr(ex->args[0], sp, truep, falsep);
      tp1 = *truep;
      fp1 = *falsep;
      checkpropzero(ex->args[1], &tp1, &fp1, 1);
      checkpropsexpr(ex->args[1], sp, &tp1, &fp1);
      checkpropzero(ex->args[2], truep, falsep, 0);
      checkpropsexpr(ex->args[2], sp, truep, falsep);
      *truep &= tp1;
      *falsep &= fp1;
      break;

  case EK_VAR:
  case EK_NAME:
  case EK_DOT:
      for (i = 0; i < numpropchecks; i++) {
    if (exprsame(ex, flowexprs[i], 2)) {
        if (flowprops[i] != PROP_INVALID &&
      (*truep | *falsep) & (1L << i))
      flowuseful = 1;
        flowassigns[i] = NULL;
    }
      }
      break;

  default:
      break;
    }
}
//}}}
//{{{
Static long breaktrue, breakfalse, conttrue, contfalse;

Static void checkprops(sp, truep, falsep)
Stmt *sp;
long *truep, *falsep;
{
    Stmt *sp1;
    int i, j;
    long tp1, fp1, tp2, fp2;
    long savebt, savebf, savect, savecf;

    while (sp) {
  if (flowrecord) {
      sp->trueprops = *truep;
      sp->falseprops = *falsep;
  }
  switch (sp->kind) {

      case SK_ASSIGN:
    checkpropsexpr(sp->exp1, sp, truep, falsep);
    if (flowrecord) {
        sp->trueprops &= *truep;
        sp->falseprops &= *falsep;
    }
    break;

      case SK_CASE:
    checkpropsexpr(sp->exp1, sp, truep, falsep);
    if (flowrecord) {
        sp->trueprops &= *truep;
        sp->falseprops &= *falsep;
    }
    sp1 = sp->stm1;
    while (sp1 && sp1->kind == SK_CASELABEL) {
        tp1 = *truep;
        fp1 = *falsep;
        checkprops(sp1->stm1, &tp1, &fp1);
        *truep &= tp1;
        *falsep &= fp1;
        sp1 = sp1->next;
    }
    if (sp1) {
        tp1 = *truep;
        fp1 = *falsep;
        checkprops(sp1, &tp1, &fp1);
        *truep &= tp1;
        *falsep &= fp1;
    }
    break;

      case SK_IF:
    checkpropsexpr(sp->exp1, sp, truep, falsep);
    if (flowrecord) {
        sp->trueprops &= *truep;
        sp->falseprops &= *falsep;
    }
    if (deadendblock(sp->stm1, flowbreaks)) {
        tp1 = *truep;
        fp1 = *falsep;
        checkpropzero(sp->exp1, &tp1, &fp1, 1);
        checkprops(sp->stm1, &tp1, &fp1);
        checkpropzero(sp->exp1, truep, falsep, 0);
        checkprops(sp->stm2, truep, falsep);
    } else if (deadendblock(sp->stm2, flowbreaks)) {
        tp1 = *truep;
        fp1 = *falsep;
        checkpropzero(sp->exp1, &tp1, &fp1, 0);
        checkprops(sp->stm2, &tp1, &fp1);
        checkpropzero(sp->exp1, truep, falsep, 1);
        checkprops(sp->stm1, truep, falsep);
    } else {
        tp1 = *truep;
        fp1 = *falsep;
        checkpropzero(sp->exp1, &tp1, &fp1, 1);
        checkprops(sp->stm1, &tp1, &fp1);
        checkpropzero(sp->exp1, truep, falsep, 0);
        checkprops(sp->stm2, truep, falsep);
        *truep &= tp1;
        *falsep &= fp1;
    }
    break;

      case SK_FOR:
    checkpropsexpr(sp->exp1, sp, truep, falsep);
    checkpropsexpr(sp->exp2, sp, truep, falsep);
    checkpropsexpr(sp->exp3, sp, truep, falsep);
    if (flowrecord) {
        sp->trueprops &= *truep;
        sp->falseprops &= *falsep;
    }
    savebt = breaktrue;
    savebf = breakfalse;
    savect = conttrue;
    savecf = contfalse;
    i = flowbreaks;
    flowbreaks = 0;
    do {
        tp1 = *truep;
        fp1 = *falsep;
        tp2 = *truep;
        fp2 = *falsep;
        breaktrue = -1;
        breakfalse = -1;
        conttrue = -1;
        contfalse = -1;
        checkprops(sp->stm1, &tp1, &fp1);
        *truep &= tp1 & conttrue;
        *falsep &= fp1 & contfalse;
    } while (*truep != tp2 || *falsep != fp2);
    flowbreaks = i;
    *truep &= breaktrue;
    *falsep &= breakfalse;
    breaktrue = savebt;
    breakfalse = savebf;
    conttrue = savect;
    contfalse = savecf;
    break;

      case SK_WHILE:
    checkpropsexpr(sp->exp1, sp, truep, falsep);
    savebt = breaktrue;
    savebf = breakfalse;
    savect = conttrue;
    savecf = contfalse;
    i = flowbreaks;
    flowbreaks = 1;
    j = flowrecord;
    tp1 = *truep;
    fp1 = *falsep;
    do {
        tp2 = tp1;
        fp2 = fp1;
        breaktrue = -1;
        breakfalse = -1;
        conttrue = -1;
        contfalse = -1;
        checkpropzero(sp->exp1, &tp1, &fp1, 1);
        checkprops(sp->stm1, &tp1, &fp1);
        tp1 &= *truep & conttrue;
        fp1 &= *falsep & contfalse;
    } while (tp1 != tp2 || fp1 != fp2);
    flowrecord = 0;
    flowbreaks = 0;
    tp1 = *truep;
    fp1 = *falsep;
    checkprops(sp->stm1, &tp1, &fp1);
    *truep &= tp1 & breaktrue;
    *falsep &= fp1 & breakfalse;
    breaktrue = savebt;
    breakfalse = savebf;
    conttrue = savect;
    contfalse = savecf;
    flowbreaks = i;
    flowrecord = j;
    if (flowrecord) {
        sp->trueprops &= *truep;
        sp->falseprops &= *falsep;
    }
    break;

      case SK_REPEAT:
    savebt = breaktrue;
    savebf = breakfalse;
    savect = conttrue;
    savecf = contfalse;
    i = flowbreaks;
    flowbreaks = 0;
    do {
        breaktrue = -1;
        breakfalse = -1;
        conttrue = -1;
        contfalse = -1;
        tp1 = *truep;
        fp1 = *falsep;
        tp2 = *truep;
        fp2 = *falsep;
        checkprops(sp->stm1, &tp1, &fp1);
        checkpropsexpr(sp->exp1, sp, &tp1, &fp1);
        *truep &= tp1 & conttrue;
        *falsep &= fp1 & contfalse;
    } while (*truep != tp2 || *falsep != fp2);
    *truep &= breaktrue;
    *falsep &= breakfalse;
    breaktrue = savebt;
    breakfalse = savebf;
    conttrue = savect;
    contfalse = savecf;
    flowbreaks = i;
    if (flowrecord) {
        sp->trueprops &= *truep;
        sp->falseprops &= *falsep;
    }
    break;

      case SK_BREAK:
    breaktrue &= *truep;
    breakfalse &= *falsep;
    break;

      case SK_CONTINUE:
    conttrue &= *truep;
    contfalse &= *falsep;
    break;

      case SK_LABEL:
    *truep = 0;
    *falsep = 0;
    break;

      case SK_TRY:
    checkprops(sp->stm1, truep, falsep);
    tp1 = *truep;
    fp1 = *falsep;
    checkprops(sp->stm2, &tp1, &fp1);
    *truep &= tp1;
    *falsep &= fp1;
    break;

      default:
    checkpropsexpr(sp->exp1, sp, truep, falsep);
    checkpropsexpr(sp->exp2, sp, truep, falsep);
    checkpropsexpr(sp->exp3, sp, truep, falsep);
    checkprops(sp->stm1, truep, falsep);
    checkprops(sp->stm2, truep, falsep);
    break;

  }
  sp = sp->next;
    }
}
//}}}
//{{{
Static int checkelimexpr (ex, ex2)
Expr *ex, *ex2;
{
    int i;
    Expr *ex3 = ex2;

    if (!ex)
  return 0;
    if (ex->kind == EK_ASSIGN && exprsame(ex->args[0], ex2, 2))
  return checkelimexpr(ex->args[1], ex2);
    for (;;) {
  if (exprsame(ex, ex3, 2))
      return 1;
  if (ex->kind == EK_DOT && ex3->kind == EK_DOT &&
      exprsame(ex->args[0], ex3->args[0], 2))
      return 0;
  if (ex3->kind == EK_DOT)
      ex3 = ex3->args[0];
  else
      break;
    }
    for (i = 0; i < ex->nargs; i++)
  if (checkelimexpr(ex->args[i], ex2))
      return 1;
    return 0;
}
//}}}
//{{{
Static int checkelims (sp, ex, depth, last)
Stmt *sp;
Expr *ex;
int depth, last;
{
    int used;
    int i;

    if (!sp)
  return last;
    if (depth == 200)
  return -1;
    used = checkelims(sp->next, ex, depth+1, last);
    switch (sp->kind) {

  case SK_ASSIGN:
      if (!used) used = checkelimexpr(sp->exp1, ex);
      if (sp->exp1->kind == EK_ASSIGN &&
    exprsame(sp->exp1->args[0], ex, 2)) {
    if (!used && flowrecord && (elimdeadcode || sp->quietelim)) {
        if (sp->serial >= 0)
      curserial = sp->serial;
        if (!sp->quietelim)
      note("Eliminated unused assignment statement [338]");
        if (sp->exp1->args[0]->kind == EK_VAR)
      ((Meaning *)sp->exp1->args[0]->val.i)->refcount--;
        sp->exp1 = grabarg(sp->exp1, 1);
        flowuseful = 1;
    } else
        used = checkelimexpr(sp->exp1->args[1], ex);
      }
      break;

  case SK_GOTO:
      used = -1;
      break;

  case SK_FOR:
      i = flowrecord;
      flowrecord = 0;
      if (checkelims(sp->stm1, ex, 0, used))
    used = -1;
      flowrecord = i;
      used |= checkelims(sp->stm1, ex, 0, used);
      used |= checkelimexpr(sp->exp2, ex);
      used |= checkelimexpr(sp->exp3, ex);
      if (sp->exp1 && sp->exp1->kind == EK_ASSIGN &&
    exprsame(sp->exp1->args[0], ex, 2))
    used = 0;
      used |= checkelimexpr(sp->exp1, ex);
      break;

  case SK_WHILE:
  case SK_REPEAT:
      used |= checkelimexpr(sp->exp1, ex);
      i = flowrecord;
      flowrecord = 0;
      if (checkelims(sp->stm1, ex, 0, used))
    used = -1;
      flowrecord = i;
      used |= checkelims(sp->stm1, ex, 0, used);
      break;

  case SK_IF:
      used = (checkelims(sp->stm1, ex, 0, used) |
        checkelims(sp->stm2, ex, 0, used));
      used |= checkelimexpr(sp->exp1, ex);
      break;

  case SK_RETURN:
      used = checkelimexpr(sp->exp1, ex);
      break;

  default:
      used |= (checkelims(sp->stm1, ex, 0, used) |
         checkelims(sp->stm2, ex, 0, used));
      used |= checkelimexpr(sp->exp1, ex);
      used |= checkelimexpr(sp->exp2, ex);
      used |= checkelimexpr(sp->exp3, ex);
      if (used > 0) used = -1;
      break;

    }
    return used;
}

//}}}


//{{{
void flowblock (Stmt *sp) {

  long truep = 0, falsep = 0;
  Expr *ex;
  int i;

  numpropchecks = 0;
  globalprops = 0;
  checkelimprops = 0;
  flowbreaks = 1;
  flowrecord = 1;
  flowuseful = 0;

  findpropchecks (sp);
  if (numpropchecks)
    checkprops (sp, &truep, &falsep);

  for (i = 0; i < numpropchecks; i++) {
    if ((checkelimprops & (1L << i)) && flowprops[i] != PROP_INVALID) {
      long saveserial = curserial;
      if (checkelims (sp, flowexprs[i], 0, 0) > 0) {
        ex = flowexprs[i];
        while (ex->kind == EK_DOT)
          ex = ex->args[0];
        warning (format_s ("Variable %s may be used before it is set [337]",
                           (ex->kind == EK_NAME) ? ex->val.s : ((Meaning*)ex->val.i)->name));
        }

      curserial = saveserial;
      }
    }

  if (!analyzeflow)
    numpropchecks = 0;
  }
//}}}
//{{{
void eatstmt (Stmt **spp) {

  Stmt *sp = *spp;
  *spp = sp->next;
  sp->next = NULL;
  free_stmt(sp);
  }
//}}}
//{{{
int haslabels (Stmt *sp) {

  if (!sp)
    return 0;
  if (haslabels(sp->stm1) || haslabels(sp->stm2))
    return 1;

  return (sp->kind == SK_LABEL);
  }
//}}}

//{{{
void fixblock (spp, thereturn)
Stmt **spp, *thereturn;
{
    Stmt *sp, *sp1, *sp2, *sp3, **spp2, *thisreturn;
    Expr *ex;
    Meaning *tvar;
    int save_tryblock;
    short save_tryflag;
    int i, j, de1, de2;
    long saveserial = curserial;

    while ((sp = *spp)) {
        sp2 = sp->next;
        sp->next = NULL;
        sp = fix_statement(*spp);
        if (!sp) {
            *spp = sp2;
            continue;
        }
        *spp = sp;
        for (sp3 = sp; sp3->next; sp3 = sp3->next) ;
        sp3->next = sp2;
        if (!sp->next)
            thisreturn = thereturn;
        else if (sp->next->kind == SK_RETURN ||
                 (sp->next->kind == SK_ASSIGN &&
                  isescape(sp->next->exp1)))
            thisreturn = sp->next;
        else
            thisreturn = NULL;
  if (sp->serial >= 0)
      curserial = sp->serial;
  fixexpr_stmt = sp;
        switch (sp->kind) {

            case SK_ASSIGN:
          if (sp->exp1)
        sp->exp1 = fixexpr(sp->exp1, ENV_STMT);
    if (!sp->exp1)
        intwarning("fixblock", "sp->exp1 == NULL in SK_ASSIGN");
                if (!sp->exp1 || nosideeffects(sp->exp1, 1)) {
        eatstmt(spp);
        continue;
                } else {
                    switch (sp->exp1->kind) {

                        case EK_COND:
                            *spp = makestmt_if(sp->exp1->args[0],
                                               makestmt_call(sp->exp1->args[1]),
                                               makestmt_call(sp->exp1->args[2]));
                            (*spp)->next = sp->next;
                            continue;    /* ... to fix this new if statement */

                        case EK_ASSIGN:
                            if (sp->exp1->args[1]->kind == EK_COND && usecommas != 1) {
                                *spp = makestmt_if(sp->exp1->args[1]->args[0],
                                                   makestmt_assign(copyexpr(sp->exp1->args[0]),
                                                                   sp->exp1->args[1]->args[1]),
                                                   makestmt_assign(sp->exp1->args[0],
                                                                   sp->exp1->args[1]->args[2]));
                                (*spp)->next = sp->next;
                                continue;
                            }
          if (isescape(sp->exp1->args[1])) {
                                sp->exp1 = grabarg(sp->exp1, 1);
        continue;
                            }
          if (exprsame(sp->exp1->args[0], sp->exp1->args[1], 1)) {
                              /*  *spp = sp->next;  */
                                sp->exp1 = grabarg(sp->exp1, 0);
                                continue;
                            }
          if (sp->exp1->args[1]->kind == EK_BICALL) {
        if (!strcmp(sp->exp1->args[1]->val.s,
              getfbufname) &&
            buildreads == 1 &&
            sp->next &&
            sp->next->kind == SK_ASSIGN &&
            sp->next->exp1->kind == EK_BICALL &&
            !strcmp(sp->next->exp1->val.s,
              getname) &&
            expr_has_address(sp->exp1->args[0]) &&
            similartypes(sp->exp1->args[0]->val.type,
             filebasetype(sp->exp1->args[1]->args[0]->val.type)) &&
            exprsame(sp->exp1->args[1]->args[0],
               sp->next->exp1->args[0], 1)) {
            eatstmt(&sp->next);
            ex = makeexpr_bicall_4("fread", tp_integer,
                 makeexpr_addr(sp->exp1->args[0]),
                 makeexpr_sizeof(sp->exp1->args[1]->args[1], 0),
                 makeexpr_long(1),
                 sp->exp1->args[1]->args[0]);
            FREE(sp->exp1);
            sp->exp1 = ex;
            continue;
        }
        if (!strcmp(sp->exp1->args[1]->val.s,
              chargetfbufname) &&
            buildreads != 0 &&
            sp->next &&
            sp->next->kind == SK_ASSIGN &&
            sp->next->exp1->kind == EK_BICALL &&
            !strcmp(sp->next->exp1->val.s,
              chargetname) &&
            expr_has_address(sp->exp1->args[0]) &&
            exprsame(sp->exp1->args[1]->args[0],
               sp->next->exp1->args[0], 1)) {
            eatstmt(&sp->next);
            strchange(&sp->exp1->args[1]->val.s,
                "getc");
            continue;
        }
          }
                            break;

                        case EK_BICALL:
                            if (!strcmp(sp->exp1->val.s, name_ESCAPE)) {
                                if (fixexpr_tryblock) {
                                    *spp = makestmt_assign(makeexpr_var(mp_escapecode),
                                                           grabarg(sp->exp1, 0));
                                    (*spp)->next = makestmt(SK_GOTO);
                                    (*spp)->next->exp1 = makeexpr_name(format_s(name_LABEL,
                                                                                format_d("try%d",
                                                                                         fixexpr_tryblock)),
                                                                       tp_integer);
                                    (*spp)->next->next = sp->next;
                                    fixexpr_tryflag = 1;
                                    continue;
                                }
                            } else if (!strcmp(sp->exp1->val.s, name_ESCIO)) {
                                if (fixexpr_tryblock) {
                                    *spp = makestmt_assign(makeexpr_var(mp_escapecode),
                                                           makeexpr_long(-10));
                                    (*spp)->next = makestmt_assign(makeexpr_var(mp_ioresult),
                                                                   grabarg(sp->exp1, 0));
                                    (*spp)->next->next = makestmt(SK_GOTO);
                                    (*spp)->next->next->exp1 = makeexpr_name(format_s(name_LABEL,
                                                                                      format_d("try%d",
                                                                                               fixexpr_tryblock)),
                                                                             tp_integer);
                                    (*spp)->next->next->next = sp->next;
                                    fixexpr_tryflag = 1;
                                    continue;
                                }
                            }
          if (!strcmp(sp->exp1->val.s, putfbufname) &&
        buildwrites == 1 &&
        sp->next &&
        sp->next->kind == SK_ASSIGN &&
        sp->next->exp1->kind == EK_BICALL &&
        !strcmp(sp->next->exp1->val.s,
          putname) &&
        exprsame(sp->exp1->args[0],
           sp->next->exp1->args[0], 1)) {
        eatstmt(&sp->next);
        if (!expr_has_address(sp->exp1->args[2]) ||
            sp->exp1->args[2]->val.type !=
                sp->exp1->args[1]->val.type) {
            tvar = maketempvar(sp->exp1->args[1]->val.type,
                 name_TEMP);
            sp2 = makestmt_assign(makeexpr_var(tvar),
                sp->exp1->args[2]);
            sp2->next = sp;
            *spp = sp2;
            sp->exp1->args[2] = makeexpr_var(tvar);
            freetempvar(tvar);
        }
        ex = makeexpr_bicall_4("fwrite", tp_integer,
                   makeexpr_addr(sp->exp1->args[2]),
                   makeexpr_sizeof(sp->exp1->args[1], 0),
                   makeexpr_long(1),
                   sp->exp1->args[0]);
        FREE(sp->exp1);
        sp->exp1 = ex;
        continue;
          }
          if (!strcmp(sp->exp1->val.s, charputfbufname) &&
        buildwrites != 0 &&
        sp->next &&
        sp->next->kind == SK_ASSIGN &&
        sp->next->exp1->kind == EK_BICALL &&
        !strcmp(sp->next->exp1->val.s,
          charputname) &&
        exprsame(sp->exp1->args[0],
           sp->next->exp1->args[0], 1)) {
        eatstmt(&sp->next);
        swapexprs(sp->exp1->args[0],
            sp->exp1->args[1]);
        strchange(&sp->exp1->val.s, "putc");
        continue;
          }
          if ((!strcmp(sp->exp1->val.s, resetbufname) ||
         !strcmp(sp->exp1->val.s, setupbufname)) &&
        !fileisbuffered(sp->exp1->args[0], 0)) {
        eatstmt(spp);
        continue;
          }
          ex = print_func(sp->exp1);
          if (ex && sp->next && mixwritelns &&
        sp->next->kind == SK_ASSIGN &&
        exprsame(ex, print_func(sp->next->exp1), 1) &&
        (printnl_func(sp->exp1) ||
         printnl_func(sp->next->exp1))) {
        sp->exp1 = mix_printf(sp->exp1,
                  sp->next->exp1);
        eatstmt(&sp->next);
        continue;
          }
                            break;

                        case EK_FUNCTION:
                        case EK_SPCALL:
                        case EK_POSTINC:
                        case EK_POSTDEC:
                        case EK_AND:
                        case EK_OR:
      case EK_NEW:
      case EK_DELETE:
                            break;

                        default:
                            spp2 = spp;
                            for (i = 0; i < sp->exp1->nargs; i++) {
                                *spp2 = makestmt_call(sp->exp1->args[i]);
                                spp2 = &(*spp2)->next;
                            }
                            *spp2 = sp->next;
                            continue;    /* ... to fix these new statements */

                    }
                }
                break;

            case SK_IF:
                fixblock(&sp->stm1, thisreturn);
                fixblock(&sp->stm2, thisreturn);
                if (!sp->stm1) {
                    if (!sp->stm2) {
                        sp->kind = SK_ASSIGN;
                        continue;
                    } else {
      if (sp->stm2->kind == SK_IF && sp->stm2->exp2) {
          freeexpr(sp->stm2->exp2);
          sp->stm2->exp2 = NULL;
      }
                        sp->exp1 = makeexpr_not(sp->exp1);   /* if (x) else foo  =>  if (!x) foo */
                        swapstmts(sp->stm1, sp->stm2);
      /* Ought to exchange comments for then/else parts */
                    }
                }
    /* At this point we know sp1 != NULL */
                if (thisreturn) {
                    if (thisreturn->kind == SK_WHILE) {
                        if (usebreaks) {
                            sp1 = sp->stm1;
                            while (sp1->next)
                                sp1 = sp1->next;
                            if (sp->stm2) {
        sp2 = sp->stm2;
        while (sp2->next)
            sp2 = sp2->next;
                                i = stmtcount(sp->stm1);
                                j = stmtcount(sp->stm2);
                                if (j >= breaklimit && i <= 2 && j > i*2 &&
                                    ((implies(sp->exp1, thisreturn->exp1, 0, 1) &&
              !checkexprchanged(sp->stm1, sp->exp1)) ||
             (sp1->kind == SK_ASSIGN &&
              implies(sp1->exp1, thisreturn->exp1, 0, 1)))) {
                                    sp1->next = makestmt(SK_BREAK);
                                } else if (i >= breaklimit && j <= 2 && i > j*2 &&
                                           ((implies(sp->exp1, thisreturn->exp1, 1, 1) &&
               !checkexprchanged(sp->stm2, sp->exp1)) ||
              (sp2->kind == SK_ASSIGN &&
               implies(sp2->exp1, thisreturn->exp1, 0, 1)))) {
                                    sp2->next = makestmt(SK_BREAK);
        } else if (!checkconst(sp->exp2, 1)) {
            /* not part of an else-if */
            if (j >= continuelimit) {
          sp1->next = makestmt(SK_CONTINUE);
            } else if (i >= continuelimit) {
          sp2->next = makestmt(SK_CONTINUE);
            }
        }
          } else {
                                i = stmtcount(sp->stm1);
                                if (i >= breaklimit &&
                                    implies(sp->exp1, thisreturn->exp1, 1, 1)) {
                                    sp->exp1 = makeexpr_not(sp->exp1);
                                    sp1->next = sp->next;
                                    sp->next = sp->stm1;
                                    sp->stm1 = makestmt(SK_BREAK);
                                } else if (i >= continuelimit) {
                                    sp->exp1 = makeexpr_not(sp->exp1);
                                    sp1->next = sp->next;
                                    sp->next = sp->stm1;
                                    sp->stm1 = makestmt(SK_CONTINUE);
                                }
                            }
                        }
                    } else {
                        if (usereturns) {
                            sp2 = sp->stm1;
                            while (sp2->next)
                                sp2 = sp2->next;
                            if (sp->stm2) {
                                /* if (x) foo; else bar; (return;)  =>  if (x) {foo; return;} bar; */
                                if (stmtcount(sp->stm2) >= returnlimit) {
            if (!deadendblock(sp->stm1, 1))
          sp2->next = copystmt(thisreturn);
                                } else if (stmtcount(sp->stm1) >= returnlimit) {
                                    sp2 = sp->stm2;
                                    while (sp2->next)
                                        sp2 = sp2->next;
            if (!deadendblock(sp->stm2, 1))
          sp2->next = copystmt(thisreturn);
                                }
                            } else {      /* if (x) foo; (return;)  =>  if (!x) return; foo; */
                                if (stmtcount(sp->stm1) >= returnlimit) {
                                    sp->exp1 = makeexpr_not(sp->exp1);
                                    sp2->next = sp->next;
                                    sp->next = sp->stm1;
                                    sp->stm1 = copystmt(thisreturn);
                                }
                            }
                        }
                    }
                }
                if (!checkconst(sp->exp2, 1)) {    /* not part of an else-if */
                    de1 = deadendblock(sp->stm1, 1);
                    de2 = deadendblock(sp->stm2, 1);
                    if (de2 && !de1) {
                        sp->exp1 = makeexpr_not(sp->exp1);
                        swapstmts(sp->stm1, sp->stm2);
                        de1 = 1, de2 = 0;
                    }
                    if (de1 && !de2 && sp->stm2) {
      if (sp->stm2->kind == SK_IF && sp->stm2->exp2) {
          freeexpr(sp->stm2->exp2);
          sp->stm2->exp2 = NULL;
      }
                        for (sp2 = sp->stm2; sp2->next; sp2 = sp2->next) ;
                        sp2->next = sp->next;
                        sp->next = sp->stm2;      /* if (x) ESCAPE else foo  =>  if (x) ESCAPE; foo */
                        sp->stm2 = NULL;
                    }
                }
    fixexpr_stmt = sp;
                sp->exp1 = fixexpr(sp->exp1, ENV_BOOL);
    if (checkconst(sp->exp1, 0) &&
        (elimdeadcode > 1 || sp->quietelim)) {
        if (!sp->quietelim)
      note("Eliminated \"if false\" statement [326]");
        splicestmt(sp, sp->stm2);
        continue;
    } else if (checkconst(sp->exp1, 1) &&
         (elimdeadcode > 1 || sp->quietelim)) {
        if (!sp->quietelim)
      note("Eliminated \"if true\" statement [327]");
        splicestmt(sp, sp->stm1);
        continue;
    }
                break;

            case SK_WHILE:
                if (whilefgets &&    /* handle "while eof(f) do readln(f,...)" */
        sp->stm1 &&
        sp->stm1->kind == SK_ASSIGN &&
        sp->stm1->exp1->kind == EK_BICALL &&
        !strcmp(sp->stm1->exp1->val.s, "fgets") &&
        nosideeffects(sp->stm1->exp1->args[0], 1) &&
        nosideeffects(sp->stm1->exp1->args[1], 1) &&
        nosideeffects(sp->stm1->exp1->args[2], 1)) {
        if ((sp->exp1->kind == EK_NOT &&
       sp->exp1->args[0]->kind == EK_BICALL && *eofname &&
       !strcmp(sp->exp1->args[0]->val.s, eofname) &&
       exprsame(sp->exp1->args[0]->args[0],
          sp->stm1->exp1->args[2], 1)) ||
      (sp->exp1->kind == EK_EQ &&
       sp->exp1->args[0]->kind == EK_BICALL &&
       !strcmp(sp->exp1->args[0]->val.s, "feof") &&
       checkconst(sp->exp1->args[1], 0) &&
       exprsame(sp->exp1->args[0]->args[0],
          sp->stm1->exp1->args[2], 1))) {
      sp->stm1->exp1->val.type = tp_strptr;
      sp->exp1 = makeexpr_rel(EK_NE,
            sp->stm1->exp1,
            makeexpr_nil());
      sp->stm1 = sp->stm1->next;
        }
                }
                fixblock(&sp->stm1, sp);
    fixexpr_stmt = sp;
                sp->exp1 = fixexpr(sp->exp1, ENV_BOOL);
                if (checkconst(sp->exp1, 1))
                    infiniteloop(sp);
                break;

            case SK_REPEAT:
                fixblock(&sp->stm1, NULL);
    fixexpr_stmt = sp;
                sp->exp1 = fixexpr(sp->exp1, ENV_BOOL);
                if (checkconst(sp->exp1, 1))
                    infiniteloop(sp);
                break;

            case SK_TRY:
                save_tryblock = fixexpr_tryblock;
                save_tryflag = fixexpr_tryflag;
                fixexpr_tryblock = (int)sp->exp1->val.i;
                fixexpr_tryflag = 0;
                fixblock(&sp->stm1, NULL);
                if (fixexpr_tryflag)
                    sp->exp2 = makeexpr_long(1);
                fixexpr_tryblock = save_tryblock;
                fixexpr_tryflag = save_tryflag;
                fixblock(&sp->stm2, NULL);
                break;

            case SK_BODY:
                fixblock(&sp->stm1, thisreturn);
                break;

            case SK_CASE:
                fixblock(&sp->stm1, NULL);
    fixexpr_stmt = sp;
                sp->exp1 = fixexpr(sp->exp1, ENV_EXPR);
                if (!sp->stm1) {    /* empty case */
                    sp->kind = SK_ASSIGN;
                    continue;
                } else if (sp->stm1->kind != SK_CASELABEL) {   /* default only */
                    for (sp2 = sp->stm1; sp2->next; sp2 = sp2->next) ;
                    sp2->next = sp->next;
                    sp->next = sp->stm1;
                    sp->kind = SK_ASSIGN;
                    sp->stm1 = NULL;
                    continue;
                }
                break;

            default:
                fixblock(&sp->stm1, NULL);
                fixblock(&sp->stm2, NULL);
    fixexpr_stmt = sp;
                sp->exp1 = fixexpr(sp->exp1, ENV_EXPR);
                sp->exp2 = fixexpr(sp->exp2, ENV_EXPR);
                sp->exp3 = fixexpr(sp->exp3, ENV_EXPR);
                if (sp->next &&
                    (sp->kind == SK_GOTO ||
                     sp->kind == SK_BREAK ||
                     sp->kind == SK_CONTINUE ||
                     sp->kind == SK_RETURN) &&
                    !haslabels(sp->next)) {
                    if (elimdeadcode || sp->quietelim) {
      if (!sp->quietelim)
          note("Deleting unreachable code [255]");
                        while (sp->next && !haslabels(sp->next))
                            eatstmt(&sp->next);
                    } else {
                        note("Code is unreachable [256]");
                    }
                } else if (sp->kind == SK_RETURN &&
                           thisreturn &&
                           thisreturn->kind == SK_RETURN &&
                           exprsame(sp->exp1, thisreturn->exp1, 1)) {
                    eatstmt(spp);
        continue;
                }
                break;
        }
        spp = &sp->next;
    }
    saveserial = curserial;
}
//}}}
//}}}
//{{{  checks
//{{{
/* Convert comma expressions into multiple statements */
Static int checkcomma_expr (Stmt **spp, Expr **exp) {

  Stmt *sp;
  Expr *ex = *exp;
  int i, res;

  switch (ex->kind) {
    //{{{
    case EK_COMMA:
      if (spp) {
        res = checkcomma_expr(spp, &ex->args[ex->nargs-1]);
        for (i = ex->nargs-1; --i >= 0; ) {
          sp = makestmt(SK_ASSIGN);
          sp->exp1 = ex->args[i];
          sp->next = *spp;
          sp->quietelim = (*spp)->quietelim;
          *spp = sp;
          res = checkcomma_expr(spp, &ex->args[i]);
          }
        *exp = ex->args[ex->nargs-1];
        }
      return 1;
    //}}}
    //{{{
    case EK_COND:
      if (isescape(ex->args[1]) && spp && !isescape(ex->args[2])) {
        swapexprs(ex->args[1], ex->args[2]);
        ex->args[0] = makeexpr_not(ex->args[0]);
        }

      if (isescape(ex->args[2])) {
        if (spp) {
          res = checkcomma_expr(spp, &ex->args[1]);
          if (ex->args[0]->kind == EK_ASSIGN) {
            sp = makestmt(SK_ASSIGN);
            sp->exp1 = copyexpr(ex->args[0]);
            sp->next = makestmt(SK_IF);
            sp->next->next = *spp;
            *spp = sp;
            res = checkcomma_expr(spp, &sp->exp1);
            ex->args[0] = grabarg(ex->args[0], 0);
            sp = sp->next;
            }
          else {
            sp = makestmt(SK_IF);
            sp->next = *spp;
            *spp = sp;
            }

          sp->exp1 = makeexpr_not(ex->args[0]);
          sp->stm1 = makestmt(SK_ASSIGN);
          sp->stm1->exp1 = eatcasts(ex->args[2]);

          res = checkcomma_expr(&sp->stm1, &ex->args[2]);
          res = checkcomma_expr(spp, &sp->exp1);
          *exp = ex->args[1];
          }
        return 1;
        }

      return checkcomma_expr(spp, &ex->args[0]);
    //}}}

    case EK_AND:
    case EK_OR:
      return checkcomma_expr(spp, &ex->args[0]);

    default:
      res = 0;
      for (i = ex->nargs; --i >= 0; ) {
        res += checkcomma_expr(spp, &ex->args[i]);
        }
      return res;
    }

  }
//}}}
//{{{
Static void checkcommas (Stmt **spp) {

  Stmt *sp;
  int res;

  while ((sp = *spp)) {
    checkcommas(&sp->stm1);
    checkcommas(&sp->stm2);

    switch (sp->kind) {
      case SK_ASSIGN:
      case SK_IF:
      case SK_CASE:
      case SK_RETURN:
        if (sp->exp1)
          res = checkcomma_expr(spp, &sp->exp1);
        break;

      case SK_WHILE:
        /* handle the argument */
        break;

      case SK_REPEAT:
        /* handle the argument */
        break;

      case SK_FOR:
        if (sp->exp1)
           res = checkcomma_expr(spp, &sp->exp1);
            /* handle the other arguments */
        break;

      default:
        break;
      }

    spp = &sp->next;
    }
  }
//}}}
//{{{
Static int checkvarchangeable (Expr *ex, Meaning *mp) {

  switch (ex->kind) {
    case EK_VAR:
      return (mp == (Meaning *)ex->val.i);

    case EK_DOT:
    case EK_INDEX:
      return checkvarchangeable(ex->args[0], mp);

    default:
      return 0;
    }
  }
//}}}
//{{{
int checkvarchangedexpr (Expr *ex, Meaning *mp, int addrokay) {

  int i;
  Meaning *mp3;
  unsigned int safemask = 0;

  switch (ex->kind) {
    case EK_FUNCTION:
    //{{{
    case EK_SPCALL:
      if (ex->kind == EK_FUNCTION) {
        i = 0;
        mp3 = ((Meaning *)ex->val.i)->type->fbase;
        }
      else {
        i = 1;
        if (ex->args[0]->val.type->kind != TK_PROCPTR)
          return 1;
        mp3 = ex->args[0]->val.type->basetype->fbase;
        }

      for ( ; i < ex->nargs && i < 16; i++) {
        if (!mp3) {
          intwarning("checkvarchangedexpr", "Too many arguments for EK_FUNCTION [266]");
          break;
          }

        if (mp3->kind == MK_PARAM &&
            (mp3->type->kind == TK_ARRAY ||
             mp3->type->kind == TK_STRING ||
             mp3->type->kind == TK_SET))
          safemask |= 1<<i;
        if (mp3->isref && checkvarchangeable(ex->args[i], mp))
          return 1;
        if (mp3->kind == MK_VARPARAM && mp3->type == tp_strptr && mp3->anyvarflag)
          i++;

        mp3 = mp3->xnext;
        }

      if (mp3)
        intwarning("checkvarchangedexpr", "Too few arguments for EK_FUNCTION [267]");

      break;
    //}}}
    //{{{
    case EK_VAR:
      if (mp == (Meaning *)ex->val.i) {
        if ((mp->type->kind == TK_ARRAY ||
             mp->type->kind == TK_STRING ||
             mp->type->kind == TK_SET) &&
             ex->val.type->kind == TK_POINTER && !addrokay)
          return 1;   /* must be an implicit & */
          }
        break;
    //}}}
    case EK_ADDR:
    case EK_ASSIGN:
    case EK_POSTINC:
    //{{{
    case EK_POSTDEC:
      if (checkvarchangeable(ex->args[0], mp))
        return 1;
      break;
    //}}}
    //{{{
    case EK_BICALL:
      if (structuredfunc(ex) && checkvarchangeable(ex->args[0], mp))
        return 1;

      safemask = safemask_bicall(ex->val.s);
      break;
        /* In case calls to these functions were lazy and passed
           the array rather than its (implicit) address.  Other
           BICALLs had better be careful about their arguments. */
    //}}}
    //{{{
    case EK_PLUS:
      if (addrokay)         /* to keep from being scared by pointer */
        safemask = ~0;    /*  arithmetic on string being passed */
      break;                /*  to functions. */
    //}}}
    default:
      break;
    }

  for (i = 0; i < ex->nargs; i++) {
    if (checkvarchangedexpr(ex->args[i], mp, safemask&1))
      return 1;
    safemask >>= 1;
    }

  return 0;
  }
//}}}
//{{{
int checkvarchanged (Stmt *sp, Meaning *mp) {

  if (mp->constqual)
    return 0;

  if (mp->varstructflag || !mp->ctx || mp->ctx->kind != MK_FUNCTION ||
      mp->volatilequal || alwayscopyvalues)
    return 1;

  while (sp) {
    if (/* sp->kind == SK_GOTO || */
        sp->kind == SK_LABEL ||
        checkvarchanged(sp->stm1, mp) ||
        checkvarchanged(sp->stm2, mp) ||
        (sp->exp1 && checkvarchangedexpr(sp->exp1, mp, 1)) ||
        (sp->exp2 && checkvarchangedexpr(sp->exp2, mp, 1)) ||
        (sp->exp3 && checkvarchangedexpr(sp->exp3, mp, 1)))
      return 1;
    sp = sp->next;
    }

  return 0;
  }
//}}}
//{{{
int checkexprchanged (Stmt *sp, Expr *ex) {

  Meaning *mp;
  int i;
  for (i = 0; i < ex->nargs; i++) {
    if (checkexprchanged(sp, ex->args[i]))
      return 1;
    }

  switch (ex->kind) {
    case EK_VAR:
      mp = (Meaning *)ex->val.i;
      if (mp->kind == MK_CONST)
        return 0;
      else
        return checkvarchanged(sp, mp);

    case EK_HAT:
    case EK_INDEX:
    case EK_SPCALL:
      return 1;

    case EK_FUNCTION:
    case EK_BICALL:
      return !nosideeffects_func(ex);

    default:
      return 0;
    }
  }
//}}}

#define BadOffset  (-999)
Static int theoffset, numoffsets, numzerooffsets;
//{{{
void checkvaroffsetexpr (Expr *ex, Meaning *mp, int myoffset) {
// Check if a variable always occurs with a certain offset added, e.g. "i+1"

  int i, nextoffset = 0;
  Expr *ex2;

  if (!ex)
    return;

  switch (ex->kind) {
    //{{{
    case EK_VAR:
      if (ex->val.i == (int64_t)mp) {
        if (myoffset == 0)
          numzerooffsets++;
        else if (numoffsets == 0 || myoffset == theoffset) {
          theoffset = myoffset;
          numoffsets++;
          }
        else
          theoffset = BadOffset;
        }
      break;
    //}}}
    //{{{
    case EK_PLUS:
      ex2 = ex->args[ex->nargs-1];
      if (ex2->kind == EK_CONST && ex2->val.type->kind == TK_INTEGER) {
        nextoffset = (int)ex2->val.i;
        }
      break;
    //}}}

    case EK_HAT:
    case EK_POSTINC:
    case EK_POSTDEC:
      nextoffset = BadOffset;
      break;

    case EK_ASSIGN:
      checkvaroffsetexpr (ex->args[0], mp, BadOffset);
      checkvaroffsetexpr (ex->args[1], mp, 0);
      return;

    default:
      break;
    }

  i = ex->nargs;
  while (--i >= 0)
    checkvaroffsetexpr (ex->args[i], mp, nextoffset);
  }
//}}}
//{{{
void checkvaroffsetstmt (Stmt *sp, Meaning *mp) {

  while (sp) {
    checkvaroffsetstmt(sp->stm1, mp);
    checkvaroffsetstmt(sp->stm1, mp);
    checkvaroffsetexpr(sp->exp1, mp, 0);
    checkvaroffsetexpr(sp->exp2, mp, 0);
    checkvaroffsetexpr(sp->exp3, mp, 0);
    sp = sp->next;
    }
  }
//}}}
//{{{
int checkvaroffset (Stmt *sp, Meaning *mp) {

  if (mp->varstructflag || !mp->ctx || mp->ctx->kind != MK_FUNCTION)
    return 0;

  numoffsets = 0;
  numzerooffsets = 0;
  checkvaroffsetstmt (sp, mp);

  if (numoffsets == 0 || theoffset == BadOffset || numoffsets <= numzerooffsets * 3)
    return 0;
  else
    return theoffset;
  }
//}}}
//}}}

//{{{
Expr* initfilevar (Expr* ex) {

  Expr *ex2;
  Meaning *mp;
  char *name;

  if (ex->val.type->kind == TK_BIGFILE) {
    ex2 = copyexpr (ex);
    if (ex->kind == EK_VAR &&
        (mp = (Meaning *)ex->val.i)->kind == MK_VAR &&
        mp->ctx->kind != MK_FUNCTION &&
        !is_std_file(ex) &&
        literalfilesflag > 0 &&
        (literalfilesflag == 1 || strlist_cifind (literalfiles, mp->name)))
      name = mp->name;
    else
      name = "";
    return makeexpr_comma (makeexpr_assign (filebasename (ex), makeexpr_nil()),
                           makeexpr_assign (makeexpr_dotq (ex2, "name", tp_str255),
                           makeexpr_string (name)));
    }
  else
    return makeexpr_assign(ex, makeexpr_nil());
  }
//}}}
//{{{
void initfilevars (mp, sppp, exbase)
Meaning *mp;
Stmt ***sppp;
Expr *exbase;
{
    Stmt *sp, *sp2;
    Type *tp;
    Expr *ex;

    while (mp) {
  if ((mp->kind == MK_VAR && mp->refcount > 0 && !mp->istemporary) ||
      mp->kind == MK_FIELD) {
      tp = mp->type;
      if (isfiletype(tp, -1)) {
    mp->refcount++;
    sp = makestmt(SK_ASSIGN);
    if (exbase)
        ex = makeexpr_dot(copyexpr(exbase), mp);
    else
        ex = makeexpr_var(mp);
    sp->exp1 = initfilevar(copyexpr(ex));
    sp->quietelim = 1;
    checkcommas(&sp);
    sp2 = sp;
    while (sp2->next) sp2 = sp2->next;
    sp2->next = **sppp;
    **sppp = sp;
      } else if (tp->kind == TK_RECORD) {
    if (exbase)
        ex = makeexpr_dot(copyexpr(exbase), mp);
    else
        ex = makeexpr_var(mp);
    initfilevars(tp->fbase, sppp, ex);
    freeexpr(ex);
      } else if (tp->kind == TK_ARRAY) {
    while (tp->kind == TK_ARRAY)
        tp = tp->basetype;
    if (isfiletype(tp, -1))
        note(format_s("Array of files %s should be initialized [257]",
          mp->name));
      }
  }
  mp = mp->cnext;
    }
}
//}}}

#define FINDINITMAX 100
Static Stmt* findinittab[FINDINITMAX];
Static int findinitstep[FINDINITMAX];
Static int findinitokay;
//{{{
Static void findinitsexpr (Expr* ex) {

  int i;
  for (i = 0; i < ex->nargs; i++)
    findinitsexpr(ex->args[i]);

  if (ex->kind == EK_VAR && ((Meaning *)ex->val.i)->kind == MK_VAR)
    ((Meaning *)ex->val.i)->fakeparam = 1;
  }
//}}}
//{{{
Static int findinitscheckexpr (Expr* ex, Meaning* mp) {

  if (ex->kind == EK_VAR && (Meaning *)ex->val.i == mp)
    return 1;

  int i;
  for (i = 0; i < ex->nargs; i++) {
    if (findinitscheckexpr(ex->args[i], mp))
      return 1;
    }

  return 0;
  }
//}}}
//{{{
Static int findinitscheckstmt (Stmt* sp, Meaning* mp) {

  while (sp) {
    if (sp->exp1 && findinitscheckexpr(sp->exp1, mp))
      return 1;
    if (sp->exp2 && findinitscheckexpr(sp->exp2, mp))
      return 1;
    if (sp->exp3 && findinitscheckexpr(sp->exp3, mp))
      return 1;
    if (sp->stm1 && findinitscheckstmt(sp->stm1, mp))
      return 1;
    if (sp->stm2 && findinitscheckstmt(sp->stm2, mp))
      return 1;
    sp = sp->next;
    }

  return 0;
  }
//}}}
//{{{
Static int anygotos (Stmt* sp) {

  while (sp) {
    if (sp->kind == SK_GOTO)
      return 1;

    if (anygotos(sp->stm1) || anygotos(sp->stm2))
      return 1;

    sp = sp->next;
    }

  return 0;
  }
//}}}
//{{{
Static void findinits (Stmt* sp, int depth, int okay) {

  Meaning *mp;
  Expr *ex;
  Stmt *sprev = NULL;
  int i, sofar = 1;

  if (depth == FINDINITMAX)
    findinitokay = 0;

  while (sp) {
    if (!findinitokay)
      return;

    findinittab[depth] = sp;
    findinitstep[depth] = 0;
    sofar--;

    switch (sp->kind) {
      case SK_ASSIGN:
      //{{{
      case SK_FOR:
        if (sp->exp1 && sp->exp1->kind == EK_ASSIGN &&
            sp->exp1->args[0]->kind == EK_VAR &&
            (sp->kind == SK_ASSIGN || useinits > 2)) {
          mp = (Meaning*)sp->exp1->args[0]->val.i;

          findinitsexpr (sp->exp1->args[1]);
          if (mp->kind == MK_VAR && mp->ctx == curctx &&
              !mp->fakeparam && !mp->varstructflag && !mp->constdefn &&
              !mp->isforward && !mp->isfunction) {
            for (i = 0; i < depth; i++) {
              if (findinitscheckstmt(findinittab[i]->next, mp))
                break;
              if (findinitstep[i] == 0 && findinitscheckstmt(findinittab[i]->stm2, mp))
                break;
              }

            if (i == depth) {
              mp->fakeparam = 1;
              if (depth == 0 && sp->kind == SK_ASSIGN &&
                  sprev && !mp->wasdeclared &&
                  (isconstantexpr(sp->exp1->args[1]) ||
                  (sp->exp1->args[1]->kind == EK_VAR &&
                  sofar >= 0 && mp->type->kind != TK_RECORD &&
                  ((Meaning *)sp->exp1->args[1]->val.i)->kind == MK_PARAM)) &&
                (useinits < 3 || tinyexpr(sp->exp1->args[1]))) {
                mp->constdefn = sp->exp1->args[1];
                sprev->next = sp->next;
                sp = sprev;
                sofar++;
                }
              else if (!okay || useinits < 2 ||
                       (useinits < 3 && (sofar < 0 || depth > 0 ||
                       mp->ctx->kind == MK_MODULE || mp->ctx->varstructflag))) {
                mp->wasdeclared = 0;
                }
              else {
                mp->wasdeclared = 1;
                sp->doinit = 1;
                sofar++;
                }
              }

            else if (useinits >= 4 && !anygotos(sp)) {
              ex = sp->exp1->args[0];
              flowrecord = 0;
              for ( ; i < depth; i++) {
                if (checkelims(findinittab[i]->next, ex, 0, 0))
                  break;
                if (findinitstep[i] == 0 && checkelims(findinittab[i]->stm2, ex, 0, 0))
                  break;
                if (findinitstep[i] == 1 && checkelims(findinittab[i]->stm1, ex, 0, 0))
                  break;
                }

              if (i == depth) {
                if (!okay) {
                  mp->wasdeclared = 0;
                  }
                else {
                  sp->doinit = 1;
                  mp->fakeparam = 1;
                  mp->wasdeclared = 1;
                  findinits(sp->next, depth, 1);
                  mp->fakeparam = 0;
                  return;
                  }
                }
              }
            }
          }
        break;
      //}}}
      //{{{
      case SK_HEADER:
        sofar++;
        break;
      //}}}
      //{{{
      default:
        break;
      //}}}
      }

    if (sp->exp1)
      findinitsexpr(sp->exp1);
    if (sp->exp2)
      findinitsexpr(sp->exp2);
    if (sp->exp3)
      findinitsexpr(sp->exp3);

    findinits(sp->stm1, depth+1, (sp->kind != SK_CASELABEL));
    findinitstep[depth] = 1;
    findinits(sp->stm2, depth+1, 1);

    sprev = sp;
    sp = sp->next;
    }
  }
//}}}
//{{{
Static Stmt* p_body() {

  Stmt *sp, **spp, *spbody, **sppbody, *spbase, *thereturn;
  Meaning *mp;
  Expr *ex;
  int haspostamble;
  long saveserial;

  if (verbose)
    fprintf (logfile, "%s, %d/%d: took %s (in %s)\n",

  infname, inf_lnum, outf_lnum,
  curctx->name, curctx->ctx->name);
  notephase = 1;
  spp = &spbase;

  addstmt (SK_HEADER);
  sp->exp1 = makeexpr_var(curctx);
  checkkeyword(TOK_INLINE);
  if (curtok != TOK_END && curtok != TOK_BEGIN && curtok != TOK_INLINE) {
    if (curctx->kind == MK_FUNCTION || curctx->anyvarflag)
      wexpecttok (TOK_BEGIN);
    else
      wexpecttok (TOK_END);
    skiptotoken2 (TOK_BEGIN, TOK_END);
    }

  if (curtok == TOK_END) {
    gettok();
    spbody = NULL;
    }
  else
    spbody = p_stmt (NULL, SF_FUNC);  /* parse the procedure/program body */

  if (curtok == TOK_IDENT && curtokmeaning == curctx)
    gettok();    /* Modula-2 */

  notephase = 2;
  saveserial = curserial;
  curserial = 10000;
  if (curctx->kind == MK_FUNCTION) {     /* handle copy parameters */
    for (mp = curctx->type->fbase; mp; mp = mp->xnext) {
      if (!mp->othername && mp->varstructflag) {
        mp->othername = stralloc (format_s (name_COPYPAR, mp->name));
        mp->rectype = mp->type;
        addstmt(SK_ASSIGN);
        ex = makeexpr_name (mp->othername, mp->rectype);
        if (mp->isref) {
          ex = makeexpr_addr (ex);
          ex->val.type = mp->rectype;
          }

        sp->exp1 = makeexpr_assign(makeexpr_var(mp), ex);
         mp->refcount++;
        }
      else if (mp->othername) {
        if (checkvarchanged (spbody, mp)) {
          addstmt(SK_ASSIGN);
          sp->exp1 = makeexpr_assign (makeexpr_var (mp), makeexpr_hat (makeexpr_name (mp->othername, mp->rectype), 0));
          mp->refcount++;
          }
        else {
          /* don't need to copy it after all */
          strchange (&mp->othername, mp->name);
          ex = makeexpr_var (mp);
          ex->val.type = mp->rectype;
          replaceexpr (spbody, makeexpr_var (mp), makeexpr_hat (ex, 0));
          }
        }
      }
    }
  for (mp = curctx->cbase; mp; mp = mp->cnext) {
    if (mp->kind == MK_LABEL && mp->val.i) {
      addstmt(SK_IF);
      sp->exp1 = makeexpr_bicall_1 ("setjmp", tp_int, makeexpr_var (mp->xnext));
      sp->stm1 = makestmt (SK_GOTO);
      sp->stm1->exp1 = makeexpr_name (format_s(name_LABEL, mp->name), tp_integer);
      }
    }

  *spp = spbody;
  sppbody = spp;
  while (*spp)
    spp = &((*spp)->next);
  haspostamble = 0;

  initfilevars (curctx->cbase, &sppbody, NULL);
  for (mp = curctx->cbase; mp; mp = mp->cnext) {
    if (mp->kind == MK_VAR && mp->refcount > 0 && isfiletype(mp->type, -1) && !mp->istemporary) {
      if (curctx->kind != MK_MODULE || curctx->anyvarflag) {
        addstmt (SK_IF);                    /* close file variables */
        sp->exp1 = makeexpr_rel(EK_NE, filebasename (makeexpr_var(mp)), makeexpr_nil());
        sp->stm1 = makestmt (SK_ASSIGN);
        sp->stm1->exp1 = makeexpr_bicall_1 ("fclose", tp_void, filebasename (makeexpr_var(mp)));
        sp->quietelim = 1;
        }
      haspostamble = 1;
      }
    }

  thereturn = &bogusreturn;
  if (curctx->kind == MK_FUNCTION && curctx->type->basetype != tp_void) {
    if ((haspostamble || !checkreturns(&spbase, 1)) && curctx->cbase->refcount > 0) {
       /* add function return code */
      addstmt (SK_RETURN);
      sp->exp1 = makeexpr_var(curctx->cbase);
      }
    thereturn = NULL;
    }
  else if (curctx->kind == MK_MODULE && curctx->anyvarflag) {
    addstmt (SK_ASSIGN);
    sp->exp1 = makeexpr_bicall_1("exit", tp_void, makeexpr_name("EXIT_SUCCESS", tp_integer));
    thereturn = NULL;
    }

  curserial = saveserial;
  sp = makestmt (SK_BODY);
  sp->stm1 = spbase;
  fixblock(&sp, thereturn);           /* finishing touches to statements and expressions */
  if (analyzeflow) {
    flowblock (sp);
    if (analyzeflow && flowuseful)
      fixblock (&sp, thereturn);
    if (analyzeflow) {
      flowblock (sp);
      if (flowuseful)
        fixblock (&sp, thereturn);
      }
    }

  spbase = sp->stm1;
  FREE(sp);
  if (usecommas != 1)
    checkcommas (&spbase);    /* unroll ugly EK_COMMA and EK_COND expressions */
  if (useinits) {
    findinitokay = 1;
    mp = curctx->cbase;
    while (mp) {
      if (mp->kind == MK_VAR && mp->wasdeclared)
        mp->fakeparam = 1;
      mp = mp->cnext;
      }
    findinits (spbase, 0, 1);
    }

  notephase = 0;
  return spbase;
  }
//}}}

#define checkWord()  if (anywords) output(" "); anywords = 1
//{{{
Static void out_function (Meaning *func) {

  Meaning *mp;
  Symbol *sym;
  int opts, anywords, spacing, saveindent;

  if (func->varstructflag)
    makevarstruct (func);

  if (collectnest) {
    for (mp = func->cbase; mp; mp = mp->cnext)
      if (mp->kind == MK_FUNCTION && mp->isforward)
        forward_decl(mp, 0);

    for (mp = func->cbase; mp; mp = mp->cnext)
      if (mp->kind == MK_FUNCTION && mp->type && !mp->exported) {
        pushctx (mp);
        out_function (mp);    /* generate the sub-procedures first */
        popctx();
        }
    }

  spacing = functionspace;
  for (mp = func; mp->ctx->kind == MK_FUNCTION; mp = mp->ctx)
    if (spacing > minfuncspace)
      spacing--;
  outsection (spacing);

  flushcomments (&func->comments, -1, 0);
  if (usePPMacros == 1) {
    forward_decl (func, 0);
    outsection (minorspace);
    }

  opts = ODECL_HEADER;
  anywords = 0;
  if (func->namedfile) {
    checkWord();
    if (useAnyptrMacros || ansiC < 2)
      output ("Inline");
    else
      output ("inline");
    }

  if (func->bufferedfile) {
    checkWord();
    output ("virtual");
    }

  if (!func->exported) {
    if (func->ctx->kind == MK_FUNCTION) {
      if (useAnyptrMacros) {
        checkWord();
        output ("Local");
        }
      else if (use_static) {
        checkWord();
        output ("static");
        }
      }
    else if ((findsymbol (func->name)->flags & NEEDSTATIC) || (use_static != 0 && !useAnyptrMacros)) {
      checkWord();
      output ("static");
      }
    else if (use_static && useAnyptrMacros) {
      checkWord();
      output ("Static");
      }
    }

  if (func->type->basetype != tp_void || ansiC != 0) {
    checkWord();
    if (func->type->smin)
      output (func->type->smin->val.s);
    else
      outbasetype (func->type, 0);
    }

  if (anywords) {
    if (newlinefunctions)
      opts |= ODECL_FUNCTION;
    else
      opts |= ODECL_SPACE;
    }
  outdeclarator (func->type, func->name, opts);

  if (fullprototyping == 0) {
    saveindent = outindent;
    moreindent (argindent);
    out_argdecls (func->type);
    outindent = saveindent;
    }

  for (mp = func->type->fbase; mp; mp = mp->xnext)
    if (mp->othername && strcmp(mp->name, mp->othername))
      mp->wasdeclared = 0;    /* make sure we also declare the copy */

  func->wasdeclared = 1;
  outcontext = func;
  out_block ((Stmt *)func->val.i, BR_FUNCTION, 10000);
  if (useundef) {
    anywords = 0;
    for (mp = func->cbase; mp; mp = mp->cnext) {
      if (mp->kind == MK_CONST && mp->isreturn) {
        // the was-#defined flag
        if (!anywords)
          outsection (minorspace);
        anywords++;
        output (format_s ("#undef %s\n", mp->name));
        sym = findsymbol (mp->name);
        sym->flags &= ~AVOIDNAME;
        }
      }
    }

  if (conserve_mem) {
    // is this safe?
    free_stmt ((Stmt*)func->val.i);
    func->val.i = 0;
    forget_ctx (func, 0);
    }

  outsection (spacing);
}
//}}}
//{{{
void movetoend (mp)
Meaning *mp;
{
    Meaning **mpp;

    if (mp->ctx != curctx) {
        intwarning("movetoend", "curctx is wrong [268]");
    } else {
        mpp = &mp->ctx->cbase;     /* move a meaning to end of its parent context */
        while (*mpp != mp) {
      if (!*mpp) {
    intwarning("movetoend", "meaning not on its context list [269]");
    return;
      }
            mpp = &(*mpp)->cnext;
  }
        *mpp = mp->cnext;    /* Remove from present position in list */
        while (*mpp)
            mpp = &(*mpp)->cnext;
        *mpp = mp;           /* Insert at end of list */
        mp->cnext = NULL;
        curctxlast = mp;
    }
}

//}}}
//{{{
Static void scanfwdparams (Meaning* mp) {

  Symbol* sym;
  mp = mp->type->fbase;
  while (mp) {
    sym = findsymbol(mp->name);
    sym->flags |= FWDPARAM;
    mp = mp->xnext;
    }
  }
//}}}
//{{{
Static void out_include (char* name, int quoted) {

  if (*name == '"' || *name == '<')
    output (format_s ("#include %s\n", name));
   else if (quoted)
    output (format_s ("#include \"%s\"\n", name));
  else
    output (format_s ("#include <%s>\n", name));
  }
//}}}
//{{{
Static void cleanheadername (char* dest, char* name) {

  if (*name == '<' || *name == '"')
    name++;

  char* cp = my_strrchr(name, '/');
  if (cp)
    cp++;
  else
    cp = name;

  strcpy (dest, cp);

  int len = (int)strlen (dest);
  if (dest[len-1] == '>' || dest[len-1] == '"')
    dest[len-1] = 0;
  }
//}}}
//{{{
Static int tryimport (Symbol* sym, char* fname, char* ext, int need) {

  int found = 0;
  Meaning *savectx, *savectxlast;

  savectx = curctx;
  savectxlast = curctxlast;
  curctx = nullctx;

  curctxlast = curctx->cbase;
  while (curctxlast && curctxlast->cnext)
    curctxlast = curctxlast->cnext;

  if (p_search(fname, ext, need)) {
    curtokmeaning = sym->mbase;
    while (curtokmeaning && !curtokmeaning->isactive)
      curtokmeaning = curtokmeaning->snext;
    if (curtokmeaning)
      found = 1;
    }

  curctx = savectx;
  curctxlast = savectxlast;
  return found;
  }
//}}}
//{{{
void do_include (Token blkind) {

  int savelnum = outf_lnum;

  outsection (majorspace);
  flushcomments (NULL, -1, -1);

  char fname[256];
  strcpy (fname, format_s (includeoutfnfmt, curtokbuf));
  if (!strcmp (fname, codefname)) {
    warning ("Include file name conflict! [272]");
    badinclude();
    return;
    }
  flush_outfilebuf();

  FILE* oldfile = fopen (fname, "w");
  if (!outf) {
    outf = oldfile;
    perror (fname);
    badinclude();
    return;
    }

  outf_lnum = 1;
  if (nobanner)
    output ("\n");
  else
    output (format_ss("\n/* Include file %s from %s */\n\n", fname, codefname));
  if (blkind == TOK_END)
    gettok();
  else
    curtok = blkind;

  p_block (blockkind);
  if (nobanner)
    output ("\n");
  else
    output ("\n\n/* End. */\n\n");
  flush_outfilebuf();
  fclose (outf);

  outf = oldfile;
  outf_lnum = savelnum;
  if (curtok != TOK_EOF) {
    warning("Junk at end of include file ignored [273]");
   }
  outsection (majorspace);
  if (*includefnfmt)
    out_include (format_s(includefnfmt, fname), 1);
  else
    out_include (fname, 1);
  outsection (majorspace);

  pop_input();
  getaline();
  gettok();
  }
//}}}

//{{{
Static void skipunitheader() {

  if (curtok == TOK_LPAR || curtok == TOK_LBR)
    skipparens();
  }
//}}}
//{{{
Static void skiptomodule() {

  skipping_module++;
  while (curtok != TOK_MODULE) {
    if (curtok == TOK_END) {
      gettok();
      if (curtok == TOK_DOT)
        break;
      }
     else
      gettok();
    }

  skipping_module--;
  }
//}}}

//{{{
Static void p_moduleinit (Meaning* mod) {

  if (curtok != TOK_BEGIN && curtok != TOK_END) {
    wexpecttok (TOK_END);
    skiptotoken2 (TOK_BEGIN, TOK_END);
    }

  if (curtok == TOK_BEGIN || initialcalls) {
    echoprocname (mod);

    Stmt* sp = p_body();
    strlist_mix (&mod->comments, curcomments);
    curcomments = NULL;

    if (ansiC != 0)
      output ("void ");

    output (format_s (name_UNITINIT, mod->name));

    if (void_args)
      output ("(void)\n");
    else
      output ("()\n");

    outcontext = mod;
    out_block (sp, BR_FUNCTION, 10000);
    free_stmt (sp);

    /* The following must come after out_block! */
    Strlist* sl = strlist_append (&initialcalls, format_s("%s()", format_s (name_UNITINIT, mod->name)));
    sl->value = 1;
    }
  else
    needToken (TOK_END);
  }
//}}}
//{{{
Static void p_nested_module() {

  Meaning *mp;

  if (!modula2) {
    note ("Ignoring nested module [260]");
    p_module (1, 0);
    return;
    }

  note ("Nested modules not fully supported [261]");
  checkmodulewords();

  needToken (TOK_MODULE);
  wexpecttok (TOK_IDENT);

  mp = addmeaning (curtoksym, MK_MODULE);
  mp->anyvarflag = 0;

  gettok();
  skipunitheader();
  needToken (TOK_SEMI);

  p_block (TOK_IMPLEMENT);
  p_moduleinit (mp);

  if (curtok == TOK_IDENT)
    gettok();

  needToken (TOK_SEMI);
  }
//}}}
//{{{
Static int p_module (int ignoreit, int isdefn) {
/* Modula-2: 0=local module, 1=DEFINITION, 2=IMPLEMENTATION */

  Meaning *mod, *mp;
  Strlist *sl;
  int kind;
  char* cp;

  checkmodulewords();
  needToken (TOK_MODULE);
  wexpecttok (TOK_IDENT);
  if (curtokmeaning && curtokmeaning->kind == MK_MODULE && isdefn == 2) {
    //{{{  module
    mod = curtokmeaning;
    import_ctx (mod);
    for (mp = mod->cbase; mp; mp = mp->cnext)
      if (mp->kind == MK_FUNCTION)
        mp->isforward = 1;
    }
    //}}}
  else
    mod = addmeaning (curtoksym, MK_MODULE);

  mod->anyvarflag = 0;
  pushctx (mod);
  gettok();
  skipunitheader();
  needToken (TOK_SEMI);

  if (ignoreit || (requested_module && strcicmp (requested_module, mod->name))) {
    if (!quietmode)
      if (outf == stdout)
        fprintf (stderr, "Skipping module %s\n", mod->name);
      else
        printf ("Skipping module %s\n", mod->name);

    checkmodulewords();
    while (curtok == TOK_IMPORT || curtok == TOK_FROM)
       p_import (1);
    checkmodulewords();

    if (curtok == TOK_EXPORT)
      gettok();
    strlist_empty (&curcomments);

    p_block (TOK_IMPORT);
    setup_module (mod->sym->name, 0);
    checkmodulewords();
    if (curtok == TOK_IMPLEMENT)
      skiptomodule();
    else {
      if (!needToken (TOK_END))
        skippasttoken (TOK_END);
      if (curtok == TOK_SEMI)
        gettok();
      }

    popctx();
    strlist_empty (&curcomments);
    return 0;
    }

  found_module = 1;
  if (isdefn != 2) {
    if (!*hdrfname) {
      sl = strlist_cifind (includefrom, mod->name);
      if (sl)
        cleanheadername (hdrfname, (char *)sl->value);
      else
        strcpy (hdrfname, format_ss(headerfnfmt, infname, mod->name));
      }

    hdrf = fopen (hdrfname, "w");
    if (!hdrf) {
      //{{{  couldn't open hdr
      perror (hdrfname);
      error ("Could not open output file for header");
      }
      //}}}

    outsection (majorspace);

    if (usevextern && my_strchr (name_GSYMBOL, '%'))
      output (format_s ("#define %s\n", format_s (name_GSYMBOL, mod->sym->name)));
    if (*selfincludefmt)
      cp = format_s (selfincludefmt, hdrfname);
    else
      cp = hdrfname;

    out_include (cp, quoteincludes);
    outsection (majorspace);
    select_outfile (hdrf);

    if (nobanner)
      output ("\n");
    else if (slashslash)
      output (format_ss ("// Header for module %s, generated by p2c %s\n", mod->name, P2C_VERSION));
    else
      output (format_ss ("/* Header for module %s, generated by p2c %s */\n", mod->name, P2C_VERSION));
    if (*name_HSYMBOL) {
      cp = format_s (name_HSYMBOL, mod->sym->name);
      output (format_ss ("#ifndef %s\n#define %s\n", cp, cp));
      }

    outsection (majorspace);
    checkmodulewords();
    while (curtok == TOK_IMPORT || curtok == TOK_FROM)
      p_import (0);

    checkmodulewords();
    if (curtok == TOK_EXPORT)
      gettok();

    checkmodulewords();
    while (curtok == TOK_IMPORT || curtok == TOK_FROM)
      p_import (0);

    outsection (majorspace);
    if (usevextern) {
      output (format_s("#ifdef %s\n# define vextern\n#else\n", format_s (name_GSYMBOL, mod->sym->name)));
      output ("# define vextern extern\n#endif\n");
      }

    checkmodulewords();
    p_block (TOK_EXPORT);
    flushcomments (NULL, -1, -1);
    setup_module (mod->sym->name, 1);

    outsection (majorspace);
    if (usevextern)
      output ("#undef vextern\n");
    outsection (minorspace);

    if (*name_HSYMBOL)
      output (format_s ("#endif /*%s*/\n", format_s(name_HSYMBOL, mod->sym->name)));
    if (nobanner)
      output ("\n");
    else if (slashslash)
      output ("\n// End.\n\n");
    else
      output ("\n/* End. */\n\n");

    select_outfile (codef);
    fclose (hdrf);
    *hdrfname = 0;
    redeclarevars (mod);
    declarevars (mod, 0);
    }

  checkmodulewords();
  if (curtok != TOK_END) {
    if (!modula2 && !implementationmodules)
      needToken(TOK_IMPLEMENT);
    import_ctx (mod);
    p_block (TOK_IMPLEMENT);
    flushcomments (NULL, -1, -1);
    p_moduleinit (mod);
    kind = 1;
    }
  else {
    kind = 0;
    if (!needToken (TOK_END))
      skippasttoken (TOK_END);
    }

  if (curtok == TOK_IDENT)
    gettok();
  if (curtok == TOK_SEMI)
    gettok();

  popctx();
  return kind;
  }
//}}}
//{{{
Static void p_import (int inheader) {

  Strlist* sl;
  Symbol* sym;
  char* name;
  int found, isfrom = (curtok == TOK_FROM);

  outsection (minorspace);
  do {
    gettok();
    if (!wexpecttok (TOK_IDENT)) {
      skiptotoken (TOK_SEMI);
      break;
      }

    sym = curtoksym;
    if (curtokmeaning && curtokmeaning->kind == MK_MODULE)
      found = 1;
    else if (strlist_cifind (permimports, sym->name))
      found = 2;   /* built-in module, there already! */
    else {
      found = 0;
      sl = strlist_cifind (importfrom, sym->name);
      name = (sl) ? format_none ((char *)sl->value) : NULL;
      if (name) {
        if (tryimport (sym, name, "pas", 1))
          found = 1;
        }
      else
        for (sl = importdirs; sl && !found; sl = sl->next)
          if (tryimport (sym, format_s(sl->s, curtokcase), NULL, 0))
            found = 1;
      }

    if (found == 1) {
      if (!inheader) {
        sl = strlist_cifind (includefrom, curtokmeaning->name);
        name = (sl) ? (char *)sl->value : format_ss (*headerfnfmt2 ? headerfnfmt2 : headerfnfmt, infname, curtokmeaning->name);
        if (name && !strlist_find (includedfiles, name)) {
          strlist_insert (&includedfiles, name);
          if (*name_HSYMBOL)
            output (format_s("#ifndef %s\n", format_s(name_HSYMBOL, sym->name)));
          out_include (name, quoteincludes);
          if (*name_HSYMBOL)
            output ("#endif\n");
          outsection (minorspace);
          }
        }
      import_ctx (curtokmeaning);
      }
    else if (curtokmeaning) {
      /* Modula-2, importing a single ident Ignored for now, since we always import whole modules */
      }
    else if (found == 0) {
      warning (format_s ("Could not find module %s [271]", sym->name));
      if (!inheader)
        out_include (format_ss (*headerfnfmt2?headerfnfmt2:headerfnfmt, sym->name, sym->name), quoteincludes);
      }
    gettok();
    } while (curtok == TOK_COMMA);

  if (isfrom) {
    checkkeyword (TOK_IMPORT);
    if (needToken (TOK_IMPORT)) {
      do {
        gettok();
        if (curtok == TOK_IDENT)
          gettok();
        } while (curtok == TOK_COMMA);
      }
    }
  if (!needToken (TOK_SEMI))
    skippasttoken (TOK_SEMI);

  outsection (minorspace);
  }
//}}}
//{{{
Static void p_function (int isfunc) {

  Meaning* func;
  Type* type;
  Stmt* sp;
  Strlist *sl, *comments, *savecmt;
  int initializeattr = 0, isinline = 0;
  int savewithlevel = withlevel;

  if ((sl = strlist_find (attrlist, "INITIALIZE")) != NULL) {
    //{{{  initialise
    initializeattr = 1;
    strlist_delete (&attrlist, sl);
    }
    //}}}
  if ((sl = strlist_find (attrlist, "OPTIMIZE")) != NULL &&
      sl->value != -1 && !strcmp ((char *)(sl->value), "INLINE")) {
    //{{{  optimise
    isinline = 1;
    strlist_delete(&attrlist, sl);
    }
    //}}}

  ignore_attributes();
  comments = extractcomment (&curcomments, -1, curserial);
  changecomments (comments, -1, -1, -1, 0);
  if (curctx->kind == MK_FUNCTION)
    savecmt = curcomments;
  else {
    savecmt = NULL;
    flushcomments (&curcomments, -1, -1);
    }

  curcomments = comments;
  curserial = serialcount = 1;
  gettok();

  if (!wexpecttok (TOK_IDENT))
    skiptotoken (TOK_IDENT);

  if (curtokmeaning && curtokmeaning->kind == MK_TYPE &&
      curtokmeaning->type->kind == TK_RECORD &&
      curtokmeaning->type->issigned) {
    //{{{  mk_type
    Meaning *cls, *mp;
    cls = curtokmeaning;
    gettok();
    needtok (TOK_DOT);
    if (!wexpecttok (TOK_IDENT))
      skiptotoken (TOK_IDENT);

    mp = curtoksym->fbase;
    while (mp && mp->rectype != cls->type)
      mp = mp->snext;
    if (mp) {
      func = mp->xnext;
      withrecordtype (cls->type, NULL);
      memberfuncwithlevel = withlevel;
      }
    else {
      //{{{  non existent member
      warning (format_ss ("Declaration of nonexistent member %s.%s [331]", cls->name, curtoksym->name));
      func = addmeaning (curtoksym, MK_FUNCTION);
      }
      //}}}
    skiptotoken (TOK_SEMI);
    pushctx (func);
    type = func->type;
    }
    //}}}
  else if (curtokmeaning && curtokmeaning->ctx == curctx &&
           curtokmeaning->kind == MK_FUNCTION) {
    //{{{  mk_function
    func = curtokmeaning;
    if (!func->isforward || func->val.i)
      warning (format_s ("Redeclaration of function %s [270]", func->name));
    skiptotoken (TOK_SEMI);
    movetoend (func);
    pushctx (func);
    type = func->type;
    }
    //}}}
  else {
    //{{{  who knows
    func = addmeaning (curtoksym, MK_FUNCTION);
    gettok();
    func->val.i = 0;
    pushctx (func);
    func->type = type = p_funcdecl (&isfunc, 0);
    func->isfunction = isfunc;
    func->namedfile = isinline;
    type->meaning = func;
    }
    //}}}

  if (blockkind == TOK_EXPORT)
    flushcomments (NULL, -1, -1);
  needToken (TOK_SEMI);

  if (initializeattr) {
    //{{{  initialise attrib
    sl = strlist_append (&initialcalls, format_s ("%s()", func->name));
    sl->value = 1;
    }
    //}}}
  if (curtok == TOK_IDENT && !strcmp (curtokbuf, "C")) {
    //{{{  ident
    gettok();
    needToken (TOK_SEMI);
    }
    //}}}

  if (blockkind == TOK_IMPORT) {
    //{{{  import
    strlist_empty (&curcomments);
    if (curtok == TOK_IDENT &&
        (!strcicmp (curtokbuf, "FORWARD") ||
         strlist_cifind (externwords, curtokbuf) ||
         strlist_cifind (cexternwords, curtokbuf))) {
      gettok();
      while (curtok == TOK_IDENT)
      gettok();
      needToken (TOK_SEMI);
      }
        /* do nothing more */
    }
    //}}}
  else if (blockkind == TOK_EXPORT) {
    //{{{  export
    func->isforward = 1;
    scanfwdparams (func);
    forward_decl (func, 1);
    }
    //}}}
  else {
    //{{{  who knows
    checkkeyword (TOK_INTERRUPT);
    checkkeyword (TOK_INLINE);
    if (curtok == TOK_INTERRUPT) {
      note ("Ignoring INTERRUPT keyword [258]");
      gettok();
      needToken (TOK_SEMI);
      }

    if (curtok == TOK_IDENT && !strcicmp (curtokbuf, "FORWARD")) {
      //{{{  forward
      func->isforward = 1;
      scanfwdparams (func);
      gettok();
      if (func->ctx->kind != MK_FUNCTION) {
        outsection (minorspace);
        flushcomments (NULL, -1, -1);
        forward_decl (func, 0);
        outsection (minorspace);
        }
      }
      //}}}
    else if (curtok == TOK_IDENT &&
             (strlist_cifind (externwords, curtokbuf) ||
              strlist_cifind (cexternwords, curtokbuf))) {
      //{{{  ident
      if (*externalias && my_strchr (externalias, '%')) {
        strchange(&func->name, format_s (externalias, func->name));
        }
      else if (strlist_cifind (cexternwords, curtokbuf)) {
        if (func->name[0] == '_')
          strchange(&func->name, func->name + 1);
        if (func->name[strlen(func->name)-1] == '_')
            func->name[strlen(func->name)-1] = 0;
        }
      func->isforward = 1;    /* for Oregon Software Pascal-2 */
      func->exported = 1;
      gettok();
      while (curtok == TOK_IDENT)
        gettok();
      outsection (minorspace);
      flushcomments (NULL, -1, -1);
      scanfwdparams (func);
      forward_decl (func, 1);
      outsection (minorspace);
      }
      //}}}
    else if (curtok == TOK_IDENT) {
      //{{{  more ident
      wexpecttok (TOK_BEGIN);   /* print warning */
      gettok();
      outsection (minorspace);
      flushcomments(NULL, -1, -1);
      scanfwdparams (func);
      forward_decl (func, 1);
      outsection (minorspace);
      }
      //}}}
    else {
      //{{{  who knows
      if (func->ctx->kind == MK_FUNCTION)
        func->ctx->needvarstruct = 1;
      func->comments = curcomments;
      curcomments = NULL;
      p_block (TOK_FUNCTION);
      echoprocname (func);
      changecomments (curcomments, -1, curserial, -1, 10000);
      sp = p_body();
      withlevel = savewithlevel;
      func->ctx->needvarstruct = 0;
      func->val.i = (int64_t)sp;
      strlist_mix(&func->comments, curcomments);
      curcomments = NULL;
      if (func->ctx->kind != MK_FUNCTION || !collectnest) {
         out_function (func);    /* output top-level procedures immediately */
         }                          /*  (sub-procedures are output later) */
      }
      //}}}
    if (!needToken (TOK_SEMI))
      skippasttoken (TOK_SEMI);
    }
    //}}}

  withlevel = savewithlevel;
  strlist_mix (&curcomments, savecmt);
  popctx();
  }
//}}}

//{{{
void p_block (Token blkind) {
//{{{
/* blockkind is one of:
  TOK_PROGRAM:     Global declarations of a program
  TOK_FUNCTION:    Declarations local to a procedure or function
  TOK_IMPORT:      Import text read from a module
  TOK_EXPORT:      Export section of a module
  TOK_IMPLEMENT:   Implementation section of a module
  TOK_END:         None of the above
*/
//}}}

  Token saveblockkind = blockkind;
  Token lastblockkind = TOK_END;
  blockkind = blkind;

  for (;;) {
    while (curtok == TOK_INTFONLY) {
      include_as_import();
      gettok();
      }

    if (curtok == TOK_CONST || curtok == TOK_TYPE || curtok == TOK_VAR ||
        curtok == TOK_VALUE || curtok == TOK_COMMON || curtok == TOK_ACCESS) {
      // const,type,var,value,common,access
      while (curtok == TOK_CONST || curtok == TOK_TYPE || curtok == TOK_VAR ||
             curtok == TOK_VALUE || curtok == TOK_COMMON || curtok == TOK_ACCESS) {
        lastblockkind = curtok;
        switch (curtok) {
          //{{{
          case TOK_CONST:
            p_constdecl();
            break;
          //}}}
          //{{{
          case TOK_TYPE:
            p_typedecl();
            break;
          //}}}
          //{{{
          case TOK_VAR:
            p_vardecl (0);
            break;
          //}}}
          //{{{
          case TOK_VALUE:
            p_valuedecl();
            break;
          //}}}
          //{{{
          case TOK_COMMON:
            p_commondecl();
            break;
          //}}}
          //{{{
          case TOK_ACCESS:
            skippasttoken (TOK_SEMI);
            lastblockkind = TOK_END;
            break;
          //}}}
          //{{{
          default:
            break;
          //}}}
          }
        }

      if ((blkind == TOK_PROGRAM || blkind == TOK_EXPORT || blkind == TOK_IMPLEMENT) &&
          (curtok != TOK_BEGIN || !mainlocals)) {
        outsection (majorspace);
        if (declarevars (curctx, 0))
          outsection (majorspace);
        }
      }
    else {
      checkmodulewords();
      checkkeyword (TOK_SEGMENT);
      if (curtok == TOK_SEGMENT) {
        //{{{  segment
        note ("SEGMENT or OVERLAY keyword ignored [259]");
        gettok();
        }
        //}}}

      p_attributes();
      switch (curtok) {
        //{{{
        case TOK_LABEL:
          p_labeldecl();
          break;
        //}}}
        case TOK_IMPORT:
        //{{{
        case TOK_FROM:
          p_import (0);
          break;
        //}}}
        //{{{
        case TOK_EXPORT:
          do {
            gettok();
            checkkeyword (TOK_QUALIFIED);
            if (curtok == TOK_QUALIFIED)
              gettok();

            needToken (TOK_IDENT);
            } while (curtok == TOK_COMMA);

          if (!needToken (TOK_SEMI))
            skippasttoken (TOK_SEMI);

          break;
        //}}}
        //{{{
        case TOK_MODULE:
          p_nested_module();
          break;
        //}}}
        case TOK_PROCEDURE:
        case TOK_CONSTRUCTOR:
        //{{{
        case TOK_DESTRUCTOR:
          p_function (0);
          break;
        //}}}
        //{{{
        case TOK_FUNCTION:
          p_function (1);
          break;
        //}}}
        //{{{
        case TOK_INCLUDE:
          if (blockkind == TOK_PROGRAM || blockkind == TOK_IMPLEMENT || (blockkind == TOK_FUNCTION && !collectnest))
            do_include (lastblockkind);
          else
            badinclude();

          break;
        //}}}
        //{{{
        default:
          if (curtok == TOK_BEGIN && blockkind == TOK_IMPORT) {
            warning ("BEGIN encountered in interface text [274]");
            skipparens();

            if (curtok == TOK_SEMI)
              gettok();

            break;
            }

          blockkind = saveblockkind;
          return;
        //}}}
        }
      lastblockkind = TOK_END;
      }
    }
  }
//}}}
//{{{
int p_search (char* fname, char* ext, int need) {

  int savesysprog, savecopysource;
  int outerimportmark, importmark, mypermflag;

  char infnbuf[300];
  strcpy (infnbuf, fname);
  fixfname (infnbuf, ext);

  FILE* fp = fopen (infnbuf, "r");
  if (!fp) {
    if (need)
      perror (infnbuf);
    if (logfile)
      fprintf (logfile, "Unable to open search file %s\n", infnbuf);
    return 0;
    }

  flushcomments (NULL, -1, -1);
  ignore_directives++;

  savesysprog = sysprog_flag;
  sysprog_flag |= 3;

  savecopysource = copysource;
  copysource = 0;

  // obsolete
  outerimportmark = numimports;
  importmark = push_imports();

  clearprogress();
  push_input_file (fp, infnbuf, 0);

  do {
    strlist_empty (&curcomments);
    checkmodulewords();
    permflag = 0;
    if (curtok == TOK_DEFINITION) {
      //{{{  definition
      gettok();
      checkmodulewords();
      }
      //}}}
    else if (curtok == TOK_IMPLEMENT && modula2) {
      //{{{  implement modula2
      gettok();
      checkmodulewords();
      warning("IMPLEMENTATION module in search text! [275]");
      }
      //}}}
    if (!needToken (TOK_MODULE))
      break;
    if (!wexpecttok (TOK_IDENT))
      break;

    Meaning* mod = addmeaning (curtoksym, MK_MODULE);
    mod->anyvarflag = 0;
    if (!quietmode && !showprogress)
      if (outf == stdout)
        fprintf (stderr, "import %s\n", infname);
      else
        printf ("import %s\n", infname);

    if (verbose)
      fprintf (logfile, "import %s line:%d of line:%d\n", infname, inf_lnum, outf_lnum);

    pushctx (mod);
    gettok();
    skipunitheader();
    needToken (TOK_SEMI);
    mypermflag = permflag;

    checkmodulewords();
    while (curtok == TOK_IMPORT || curtok == TOK_FROM)
      p_import (1);

    checkmodulewords();
    if (curtok == TOK_EXPORT)
      gettok();

    strlist_empty (&curcomments);
    p_block (TOK_IMPORT);
    setup_module (mod->sym->name, 0);

    if (mypermflag) {
      strlist_add (&permimports, mod->sym->name)->value = (int64_t)mod;
      perm_import (mod);
      }

    checkmodulewords();
    if (curtok == TOK_END) {
      gettok();
      if (curtok == TOK_SEMI)
        gettok();
      }
    else {
      wexpecttok (TOK_IMPLEMENT);
      if (importall)
        skiptomodule();
      }
    popctx();
    } while (curtok == TOK_MODULE);

  pop_imports (importmark);
  unimport (outerimportmark);

  sysprog_flag = savesysprog;
  copysource = savecopysource;
  ignore_directives--;
  pop_input();

  strlist_empty (&curcomments);
  clearprogress();
  return 1;
  }
//}}}

//{{{
void p_program() {

  int nummods = 0;
  int isdefn = 0;

  flushcomments (NULL, -1, -1);
  output (format_s ("#include %s\n", p2c_h_name));
  outsection (majorspace);

  p_attributes();
  ignore_attributes();

  checkmodulewords();
  if (modula2) {
    if (curtok == TOK_MODULE)
      curtok = TOK_PROGRAM;
    else if (curtok == TOK_DEFINITION) {
      isdefn = 1;
      gettok();
      checkmodulewords();
      }
    else if (curtok == TOK_IMPLEMENT) {
      isdefn = 2;
      gettok();
      checkmodulewords();
      }
    }

  switch (curtok) {
    //{{{
    case TOK_MODULE:
      if (implementationmodules)
        isdefn = 2;

      nummods = 0;
      while (curtok == TOK_MODULE) {
        if (p_module(0, isdefn)) {
          nummods++;
          if (nummods == 2 && !requested_module)
            warning("Multiple modules in one source file may not work correctly [276]");
          }
        }

      needToken(TOK_DOT);
      break;
    //}}}
    //{{{
    default: {
      Meaning* prog;
      if (curtok == TOK_PROGRAM) {
        gettok();
        if (!wexpecttok (TOK_IDENT))
          skiptotoken (TOK_IDENT);

        prog = addmeaning (curtoksym, MK_MODULE);
        gettok();
        if (curtok == TOK_LPAR) {
          while (curtok != TOK_RPAR) {
            if (curtok == TOK_IDENT && strcicmp (curtokbuf, "INPUT") && strcicmp (curtokbuf, "OUTPUT")) {
              if (literalfilesflag == 2) {
                strlist_add (&literalfiles, curtokbuf);
                }
              else if (strcicmp (curtokbuf, "KEYBOARD") && strcicmp (curtokbuf, "LISTING")) {
                note (format_s ("Unexpected name \"%s\" in program header [262]", curtokcase));
                }
              }
            gettok();
            }
          gettok();
          }

        if (curtok == TOK_LBR)
          skipparens();
        needToken (TOK_SEMI);
        }
      else
        prog = addmeaning(findsymbol("program"), MK_MODULE);

      prog->anyvarflag = 1;
      if (requested_module && strcicmp(requested_module, prog->name) &&
          strcicmp (requested_module, "program")) {
        for (;;) {
          skiptomodule();
          if (curtok == TOK_DOT)
            break;
          (void)p_module (0, 2);
          }
        gettok();
        break;
        }
      pushctx (prog);
      p_block (TOK_PROGRAM);

      echoprocname (prog);
      flushcomments (NULL, -1, -1);
      if (curtok != TOK_EOF) {
        //{{{  not eof
        nullbody = 0;
        Stmt* sp = p_body();
        if (!nullbody) {
          strlist_mix (&prog->comments, curcomments);
          curcomments = NULL;
          if (*maintype)
            output (format_s("%s ", maintype));
          if (fullprototyping > 0)
            output (format_sss ("main%s(int argc,%s%s *argv[])",
                                spacefuncs ? " " : "", spacecommas ? " " : "", charname));
          else {
            output ("main");
            if (spacefuncs)
              output (" ");
            output ("(argc,");
            if (spacecommas)
              output (" ");
            output ("argv)\n");
            singleindent (argindent);
            output ("int argc;\n");
            singleindent (argindent);
            output (format_s("%s *argv[];\n", charname));
            }
          outcontext = prog;
          out_block (sp, BR_FUNCTION, 10000);
          }

        free_stmt (sp);
        popctx();

        if (curtok == TOK_SEMI)
          gettok();
        else
          needToken (TOK_DOT);
        }
        //}}}
      break;
      }
    //}}}
    }

  if (curtok != TOK_EOF)
    warning("Junk at end of input file ignored [277]");
  }
//}}}
