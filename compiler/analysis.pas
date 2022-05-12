{<<<}
{ analys.pas - Declarations module for syntax analyzer.

  This module contains the control routine for the syntax analyzer and the code to parse the declarations.
  The external procedure "body" parses and generates intermediate file output for the bodies of the procedures.
  This is split apart in this manner to allow the code for the body and declarations to be overlayed
}
{>>>}
{<<<}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1985, 1986, 1987 Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied, modified, transferred, and used only as
  provided under a signed license agreement with Oregon Software.
  Any support purchased from Oregon Software does not apply to
  user-modified programs.  All copies of this program must display
  this notice and all copyright notices.


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Declaration Syntax/Semantic Analyzer

 Last modified by KRIS on 21-Nov-1990 15:19:40
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}
{>>>}
{$nomain}
{<<<  includes}
%include 'common.def';
%include 'token.def';
%include 'inter.def';

%include 'main.def';
%include 'scan.def';

%include 'analysis.def';
{>>>}
{<<<}
const
  { Analys pass sizing parameters}
  { where hardware vm is used, blockslow is made as small as possible }
  maxblockslow = lowanalysblocks; { number of blocks in low-space }
  entriesperblock = analysmaxnodeinblock; {entries per physical file block }
  targetzero = 0; {target machine value of zero}
  targetone = 1; {target machine value of one}
{>>>}
{<<<}
type
  tokenlengthtable = array [tokentype] of 0..10; {defines token lengths}

  alignmentrange = 0..maxalign; {possible alignment values}

  {the following type has moved to hdr.pas}
  {addressrange = 0..maxaddr;} {possible address values}
  bitrange = 0..maxbit; {possible bit indices within a word}

  { Structures to describe types and labels}
  entryptr = ^tableentry;
  tableIndex = 0..tablesize;
  scoperange = 0..deadscope;
  totalscoperange = 0..totalscopes;

  labelptr = ^labelentry;
  {<<<}
  labelentry =
    packed record
      labelvalue: pascallabelrange; {user supplied value for label}
      internalvalue: labelrange; {corresponding internal value}
      nextlabel: labelptr; {previous label defined in this block}
      maxlegalnest: integer; {used for error checking}
      definednest: integer; {nesting level, 0 if not yet defined}
      labelline: integer; {line of label decl (for error)}
      labelcolumn: columnindex; {column of label decl (for error)}
      nonlocalref: boolean; {true if ref'd by non-local goto}
    end;
  {>>>}

  setvalueblock = set of 0..maxsetord; {holds set const}
  {<<<}
  range =
    record
      minlimit, maxlimit: integer; {limits of values}
      extended: boolean; {maxlimit is > maxint}
    end;
  {>>>}
  {<<<}
  operand_range =
    record
      optimistic: range; {assumes all vars in range}
      pessimistic: range; {assumes any value allowed by storage}
    end;
  {>>>}
  {<<<}
  const_descr =
    record {describes a constant value}
      case representation: types of {values for different types}
        ints:
          (intvalue: integer; {ord(value)}
           negated: boolean {value was negated} );
        ptrs: (ptrvalue: integer {targetptr} {referenced operand} );
        reals, doubles:
          (realvalue:
             record {actual real value}
               case boolean of
                 false: (realbinary: double); {computable}
                 true: (realbuffer: realarray); {just storage}
             end);
        arrays, fields, strings:
          (pos, len: integer; {location in string file}
           stringconstflag: boolean {true if 'string'} );
        sets: (setvalue: ^setvalueblock);
    end; {const_descr}
  {>>>}

  operandtype = (constoperand, varoperand, exproperand);
  {<<<}
  operand =
    record {describes expression operand}
      typeindex: tableIndex; {type of this operand}
      oprndlen: addressrange; {length of operand in units}
      value_range: operand_range; {range of values for the operand}
      extended: boolean; {set if thiy is an extended range}
      case operandkind: operandtype of {true if constant operand}
        constoperand: (cvalue: const_descr; {value of the constant} );
        varoperand, exproperand:
          (cost: integer {estimated registers to compute operand value} )
    end;
  {>>>}

  {<<<}
  tableentry =
    packed record {symbol table entry}
      dbgsymbol: p_symbolindex; {debugger form (PDB) or table (ODB) index}
      case form: boolean of
        true:
          (size: addressrange; {size of type in units or bits}
           align: alignmentrange; {alignmentrequirements}
           packedflag: boolean; {true if user said "packed"}
           bitaddress: boolean; {true if size is bits, not units}
           containsfile: boolean; {true if type has a file component}
           extendedrange: boolean; {true if upper bound > maxint}
           disposable: boolean; {true if type can be quickly disposed of}
           case typ: types of
             subranges:
               (lowerord, upperord: integer; {user said lowerord..upperord}
                parentform: types; {"typ" value of the parent}
                parenttype: tableIndex {type of lowerord, upperord} );
             fields:
               (fieldid: integer; {scope id for symbol table search}
                nextvariant: tableIndex; {next variant record at this level}
                firstlabel: tableIndex; {head of label chain describing this variant}
                firstvariant: tableIndex; {first subvariant defined by case at level}
                tagfield: tableIndex; {name entry of tagfield, 0 if none}
                firstfield: tableIndex; {index of first field in symbol table}
                lastfield: tableIndex {index of last field in record} );
             variantlabs:
               (nextvarlab: tableIndex; {points to next label for this variant}
                varlabtype: tableIndex; {type of variant label}
                varlabvalue: integer {ord(variant value)} );
             arrays, conformantarrays, strings:
               (indextype: tableIndex; { user said array [indextype] }
                elementtype: tableIndex; { of elementtype }
                highbound, lowbound: tableIndex; {bound id's if conformant}
                stringtype: boolean; { set if a string type }
                arraymembers: addressrange; {members in the indextype}
                elementsize: addressrange {effective size of element} );
             sets:
               (constructedset: boolean; {declared or [i,j..k] ?}
                basetype: tableIndex {user said set of basetype} );
             files:
               (filebasetype: tableIndex; {user defined file of filebasetype}
                filekey: integer {used to identify file componants to travrs}
               );
             ptrs:
               (ptrtypename: tableIndex; {user defined ^ptrtypename}
                ptrkey: integer {used to identify ptr components to travrs} );
             scalars: (lastord: integer {highest value of scalar} ); );
        false:
          (nextname: tableIndex; {pointer to previous incarnation of same name}
           name: scoperange; {id of block in which declared}
           charlen: 0..linelen; {length of character string defining name}
           charindex: integer; {start of name in string file}
           lastoccurrence: totalscoperange; {last scope which accessed this
                                             unit}
           case namekind: nametype of
             typename, undeftypename:
               (typeindex: tableIndex; {points to defined type}
                refdefined: boolean {true if defined by ref function} );
             constname:
               (consttype: tableIndex; {type for this constant}
                constvalue: const_descr {value for this constant} );
             varname, fieldname, param, varparam, procparam, funcparam,
             confparam, varconfparam, boundid, undefname:
               (modified: boolean; {becomes true when value assigned}
                nestedmod: boolean; {true if modified by nested procedure}
                parammodified: boolean; {becomes true when param value changes}
                knownvalid: boolean; {set when value is known to be in range}
                programdecl: boolean; {set when declared in program header}
                varianttag: boolean; {set true if record variant tag}
                registercandidate: boolean; { true if travrs can assign to a
                                             reg}
                univparam: boolean; {true if universal parameter}
                lastinsection: boolean; {last in a parameter section}
                varalloc: allockind; {kind of allocation made}
                nextparamlink: tableIndex; {if parameter, points to next param in list}
                offset: unsignedint; {address of item within block}
                length: addressrange; {length of item}
                vartype: tableIndex; {name's type}
                sparelink: tableIndex; {for boundid, array for which it's a bound;
                                        for use/shared vars index into vartable;
                                        for fields used to pass info to debugger}
                );
             standardproc, standardfunc, directivename:
               (procid: standardids {which standard procedure} );
             procname, funcname, forwardproc, forwardfunc, externalproc,
             externalfunc:
               (functype: tableIndex; {function result type}
                funclen: addressrange; {length of resulting value}
                paramlist: tableIndex; {Index of last parameter}
                funcassigned: boolean; {true if function value defined}
                procref: proctableindex; {index into global proc data for
                                          routine} savedparamsize: addressrange {size of parameters} ); );
    end;
  {>>>}
  undefindex = 0..undeftablesize;

  tokenset = set of tokentype;
  typeset = set of types;

  {<<<}
  {The following define the virtual memory for the name table}
  nameblock =
    record
      case boolean of
        false: (physical: doublediskblock);
        true:
          (logical: array [0..entriesperblock] of tableentry; {one symbol table
             block} );
    end;
  {>>>}
  nameblockptr = ^nameblock;

  blockindex = - 1..amaxblocksin;
  {<<<}
  blockmap =
    record {Used to map block number into buffer pointer}
      blkno: blockindex; { index for seek }
      written: boolean; {set if block has been modified}
      lowblock: boolean; {set if buffer not on heap}
      buffer: nameblockptr {points to block if exists, otherwise nil}
    end;
  {>>>}
  {<<<}
  undefentry =
    record {used to record forward routines and type definitions}
      tableref: tableIndex; {symbol table entry for name}
      line: integer; {Source line on which occurs}
      column: columnindex {source column at which it occurs}
    end;
  {>>>}
  blocktype = (codeblock, withblock); {types of scope}
  {<<<}
  displayentry =
    record {used to describe a scope level}
      dbgscope: p_symbolindex; {ODB sym file entry for this scope}
      blockid: scoperange; {unique id for name searching}
      scopeid: totalscoperange; {monotonic increasing block count for scope
                                 checking}
      case blockkind: blocktype of
        withblock:
          (withoffset: addressrange; {offset of with variable}
           withpacking: boolean; {this is a packed type}
           withlevel: levelindex {level of with variable} );
        codeblock:
          (blocksize: addressrange; {size of local variables for block}
           paramsize: addressrange; {size of parameters for block}
           firststmt, {for debugger support}
           laststmt : integer; {values returned by stf_stmt}
           blockref: proctableindex; {global procedure table index}
           labellist: labelptr; {all labels defined at this level}
           oldtabletop: tableIndex; {top of table at entry, for restore at end}
           threshold: tableIndex; {last name on entry, for restore at end}
           blockname: tableIndex; {name entry for the block}
           oldundeftabletop: undefindex; {to restore undef table at end}
           namesdeclared: hashindex; {number of names defined in this block}
           highestkey: hashindex; {key of highest name declared} )
    end;
  {>>>}

  intstates = (opstate, stmtstate); {intermediate code is statement or expr}

  forstackindex = 0..fordepth;

  { declarations for kluged type to allow writing to environment file }
  proctableblock = array [0..proctableentriesperblock] of proctableentry;
  tableblock = array [0..tableentriesperblock] of tableentry;
{>>>}
{<<<}
var
  sharedPtr: sharedPtrType;
  tokenSharedPtr: tokenSharedPtrType;
  interSharedPtr: interSharedPtrType;

  { Must be in the same place in analys/travrs }
  toklengths: tokenlengthtable;

  nooverflow: integer; {kludge for lib$int_over(on/off)}
  glboverflow, glbov: boolean; {needed for function return}

  legalfunctypes: typeset; {types which a function can return}
  neverskipset, { These tokens are NEVER skipped by parser }
   begconstset, { Legal tokens which start a signed constant }
   blockheadset, { Begblockset - [beginsym] }
   begblockset, { Legal tokens which start a block }
   begparamhdr, { Legal tokens which start a param }
   nextparamhdr, { Legal tokens which start next param }
   begstmtset, { Legal tokens which start a stmt }
   begunsignedset, { Legal tokens which start unsigned consts }
   begsimplset, { Legal tokens which begin simple types }
   begstructset, { Legal tokens which start a structured type }
   begtypset, { Legal tokens which start a type def }
   begfactset, { Legal tokens which start a factor }
   constfollowset, { Tokens which may follow a constant decl }
   typefollowset, { Tokens which may follow a type decl }
   begexprset, { Legal and illegal tokens which start an expression }
   exprops, { Expression operators (relational ops) }
   sexprops, { Simple expression operators (adding ops) }
   termops: tokenset { Term operators (multiplying ops) } ;

  {token records bracketing the current, having several helps error recovery}
  thistoken, lasttoken: tokenrecord;
  token: tokentype; {current token}

  sourcestringindex: unsignedint; {pos in stringtable of source file name}

  display: array [levelindex] of displayentry; {compile time display}

  { the following are entries for standard types }
  intindex: tableIndex;
  shortintindex: tableIndex;
  realindex: tableIndex;
  doubleindex: tableIndex;
  chartypeindex: tableIndex;
  boolindex: tableIndex;
  noneindex: tableIndex;
  nilindex: tableIndex;
  textindex: tableIndex;
  inputindex: tableIndex;
  outputindex: tableIndex;
  subrangeindex: tableIndex;
  nullboundindex: tableIndex;

  emptysetgenerated: boolean; {true if '[]' already emitted}
  emptysetcount: integer; {where it is, if it is}
  inputdeclared, outputdeclared: boolean; {true if declared in program stmt}

  optable: array [eql..andsym] of operator; {maps tokens into operators}

  oprndstk: array [0..oprnddepth] of operand; {stack for expression evaluation}
  sp: - 1..oprnddepth; {top of operand stack}

  keymap: array [hashindex] of tableIndex; {Index into symboltable by name}

  stringfilebase: integer; {top of stringfile when we enter analys}

  undeftable: array [undefindex] of undefentry; {forward reference table}

  lastdebugrecord: integer;   { last record written in debugger file}
  lastprocrecord: integer;    { last procedure record written in debugger file}
  lastfilekey: integer;       { used to generate unique ids for file and ptr types}
  tabletop: tableIndex;       { last entry in symboltable}
  undeftabletop: undefindex;  { last entry in forward def table}
  displaytop: levelindex;     { top of display stack}
  labelflag: labelptr;        { used to mark end of form and label lists}

  lastid: scoperange;         { last named scope created}
  lastscope: totalscoperange; { last marker for scope checking}
  level: levelindex;          { current block level }
  lev: levelindex;            { Returned by search -- level of item found }

  nilvalue: operand; { value of reserved word nil}

  intstate: intstates;         { state of intermediate file, operator or statement}
  emitflag: boolean;           { set if intfile to be emitted, reset on error}
  checkundefs: boolean;        { set if valid to check for undef var usage}
  nolabelsofar: boolean;       { set if no labels encountered yet}
  anyfile: boolean;            { used in record parsing to see if contains file}
  anyexternals: boolean;       { set if any externals in entire compilation unit}
  anynonlocallabels: boolean;  { set if any non-local labels in this block}
  nextintcode: 0..diskbufsize; { intfile buffer pointer}
  paramlistid: integer;        { scope id for last param list}
  nowdebugging: boolean;       { current block has debugging code}

  { constant folding }
  quoflag: boolean;            { true if div is for a quotient operation, not rem}
  divfolded: boolean;          { tells "remop" or "quoop" folding that "divop" folded}
  divide_extended: boolean;    { divide left operand was extended}
  divide_range: operand_range; { range of left operand of a div}
  linearize: boolean;          { true if constants folding into array base addr}
  linearfactor: integer;       { saves const from genbinary to array index}
  skipfactor: boolean;         { true sez factor already read when expression called}

  varindex: tableIndex;        { index of latest variable parsed}
  varptr: entryptr;            { pointer to name entry for varindex}
  resulttype: tableIndex;      { type of current operation being parsed}
  resultptr: entryptr;         { pointer to type block of resulttype}
  resultform: types;           { form for resulttype}
  result_range: operand_range; { range for current operation}

  forstack: array [forstackindex] of
      record
        containedgoto: boolean; { true says for loop contained goto statement}
        forindex: tableIndex;   { controlled vars for for loops}
        forrange: range;
      end;
  forsp: forstackindex; { top of forstack}

  loopfactor: integer; { non-zero if within loop}

  { genunary and genbinary }
  foldedunary: boolean;     { set if unary operation successfully folded}
  oconst: boolean;          { set if unary op operand is constant}
  ocost, olen: integer;     { operation cost and result length for unary op}
  oextended: boolean;       { set if the operation is extended range}
  lconst, rconst: boolean;  { left or right operand constant for binary op}
  foldedbinary: boolean;    { binary folding attempt was successful}
  l, r: 0..oprnddepth;      { operand indices for folding binary op}
  c1, c2, newcost: integer; { used for computing costs of binary op}
  newlen: addressrange;     { result length for binary operation}
  unaryform, binaryform: types; { operation result types for unary, binary op}
  unaryop, binaryop: operator; { op being generated for genunary, genbinary}

  nest: integer;        { statement nesting depth for goto checking}
  jumpoutnest: integer; { outermost nesting level for jumps out of for loops}
  probing: boolean;     { set if tentative probe of symbol table, not real usage}

  anynonpascalcalls: boolean; {set if block contains any non-pascal calls}

  fewestblocks, mostblocks: 0..amaxblocksin; {monitor virt mem scheme}

  { Gross kludge to enable placement of structured constants in analys overlay: }
  structfollow: tokenset; {tokens which can follow structured constant}
  structtype: tableIndex; {type of structured constant}
  structvalue: operand; {the returned value}
  tempvars: integer; {number of locals available for register assignment}

  lastblocksin: 1..amaxblocksin;                    { last block actually allocated}
  bigtable:  array [0..bigtablesize] of tableentry; { symboltable }
  blocksin:  array [1..amaxblocksin] of blockmap;   { name blocks in memory }
  blockslow: array [1..maxblockslow] of nameblock;
{>>>}

{<<<  utils}
procedure conststructure (follow: tokenset; form: tableIndex; var value1: operand); external;
{<<<}
procedure warn (err: warning);
{ Generate an error message in the middle of the current token }

begin
  emitflag := false;
  with thistoken do
    warnat (err, line, (left + right) div 2)
end;
{>>>}
{<<<}
procedure warnbefore (err: warning);
{ Generate an error message at the center of the last token }

begin
  emitflag := false;
  with lasttoken do
    warnat (err, line, (left + right) div 2)
end;
{>>>}
{<<<}
procedure warnbetween (err: warning);
{ Generate an error message half way between the last token and the current token }

begin
  emitflag := false;
  with lasttoken do
    if line = thistoken.line then
      warnat (err, line, (right + thistoken.left + 1) div 2)
    else
      warnat (err, line, min(right + 1, linelen))
end;
{>>>}
{<<<}
procedure warnnonstandard (err: warning);
{ Generate a warning only if the standard switch is set. Used to warn of non-standard features. }

begin
  if (sharedPtr^.switchcounters[standard] > 0) then
    warn (err)
end;
{>>>}
{<<<}
procedure analysFatal (err: warning);
{ Generate a fatal warning which will terminate the compilation }

begin
  sharedPtr^.fatalflag := true;
  warn (err);
end;
{>>>}

{<<<}
procedure gettoken;
{ Read next lexical token using scan.pas scantoken routine
  global variables altered -
    thistoken -- Receives the "next" token from the token file
    lasttoken -- Receives previous value of thistoken
    token     -- Receives tokentype of thistoken
    nexttoken -- Receives tokentype of what will be the next token read from the token file.
                 This provides a one token "look ahead" for ANALYS.
}
begin
  lasttoken := thistoken;
  thistoken := tokenSharedPtr^.nexttoken;
  token := thistoken.token;

  scanToken;
end;
{>>>}

{<<<}
procedure seekstringfile (n: integer);
{ Do the equivalent of a "seek" on the string file
  This sets the file and "nextstringfile" to access byte "n" of the stringfile
}
var
  newblock: 1..maxstringblks; { block to which seeking }

begin
  newblock := n div (diskbufsize + 1) + 1;
  if newblock <> sharedPtr^.curstringblock then
    begin
    sharedPtr^.stringblkptr := sharedPtr^.stringblkptrtbl[newblock];
    if sharedPtr^.stringblkptr = nil then
      begin
      new (sharedPtr^.stringblkptr);
      sharedPtr^.stringblkptrtbl[newblock] := sharedPtr^.stringblkptr;
      end;
    sharedPtr^.curstringblock := newblock;
    end;

  sharedPtr^.nextstringfile := n mod (diskbufsize + 1);
end;
{>>>}
{<<<}
procedure getstringfile;
{ Do the equivalent of a get on the stringfile.
  The string file contains constant data collected by SCAN and ANALYS
  It is organized as blocks containing arrays 0..diskbufsize of bytes
  The string file is always accessed as stringblkptr^[nextstringfile] if caching is disabled
}
begin
  if sharedPtr^.nextstringfile = diskbufsize then
    begin
    sharedPtr^.nextstringfile := 0;
    sharedPtr^.curstringblock := sharedPtr^.curstringblock + 1;
    sharedPtr^.stringblkptr := sharedPtr^.stringblkptrtbl[sharedPtr^.curstringblock];
    end
  else
    sharedPtr^.nextstringfile := sharedPtr^.nextstringfile + 1;
end;
{>>>}
{<<<}
procedure putstringfile;
{ Do the equivalent of a "put" on the stringfile.
  The global "nextstringfile" is incremented, and if the buffer is full an actual "put" on the file is done.
  The last element is in stringblkptr^[nextstringfile]
}
begin
  if sharedPtr^.nextstringfile = diskbufsize then
    begin
    sharedPtr^.curstringblock := sharedPtr^.curstringblock + 1;
    sharedPtr^.nextstringfile := 0;
    new (sharedPtr^.stringblkptr);
    sharedPtr^.stringblkptrtbl[sharedPtr^.curstringblock] := sharedPtr^.stringblkptr;
    end
  else
    sharedPtr^.nextstringfile := sharedPtr^.nextstringfile + 1;
end;
{>>>}
{<<<}
procedure putbyte (a: integer);
{ Write the byte "a" to the next location in the string file.
  This is assumed to be added to the constant table, and the global
  "consttablelimit" or "stringfilecount" is incremented as a result.
}
begin
  sharedPtr^.stringfilecount := sharedPtr^.stringfilecount + 1;
  sharedPtr^.stringblkptr^[sharedPtr^.nextstringfile] := a;
  putstringfile;
end;
{>>>}

{<<<}
procedure putintfile;
{ Write intermediate code file.
  The output of ANALYS is an intermediate code file which is used as input to TRAVRS.
  The intermediate code file is organized as blocks containing
  arrays 0..diskbufsize of intermediate code records.
  What this routine does is to calculate an index (nextintcode) into the
  current buffer where the next intermediate code record should be written }

begin
  sharedPtr^.putlow := sharedPtr^.putlow + 1;

  if nextintcode = diskbufsize then
    begin
    nextintcode := 0;
    put (interSharedPtr^.interFile);
    end
  else
    nextintcode := nextintcode + 1;
end;
{>>>}
{<<<}
procedure genform (f: types);
{ If no errors found so far, emit a form to the intermediate file }

begin
  if emitflag then
    begin
    interSharedPtr^.interFile^.block[nextintcode].f := f;
    putintfile;
    end;
end;
{>>>}
{<<<}
procedure genint (i: integer);
{ If no errors found so far, emit an integer value to the intermediate file.
  Since each intermediate file element is only in the range 0..255 (one byte), multiple elements are used.
  Note that only unsigned integers are emitted. }

var
  { This fudges an integer into bytes.  The constant "32" is }
  { simply a large enough number to include all probable systems. }
  fudge:
    record
      case boolean of
        true: (int: integer);
        false: (byte: packed array [1..32] of hostfilebyte);
    end;
  j: 1..32; {induction var}

begin
  if emitflag then
    if (i >= 0) and (i < hostfilelim) then
      begin
      interSharedPtr^.interFile^.block[nextintcode].b := i;
      putintfile;
      end
    else
      begin
      interSharedPtr^.interFile^.block[nextintcode].b := hostfilelim;
      putintfile;
      fudge.int := i;
      for j := 1 to hostintsize * hostfileunits do
        begin
        interSharedPtr^.interFile^.block[nextintcode].b := fudge.byte[j];
        putintfile;
        end;
      end;
end;
{>>>}
{<<<}
procedure genop (o: operator);
{ If no errors are found so far, emit an operator to the intermediate file }

begin
  if emitflag then
    begin
    interSharedPtr^.interFile^.block[nextintcode].o := o;
    putintfile;
    end;
end;
{>>>}
{<<<}
procedure genstmt (s: stmttype);
{ If no errors are found so far, emit a statement to the intermediate file }

begin
  if emitflag then
    begin
    if intstate = opstate then
      begin
      genop (endexpr);
      intstate := stmtstate
      end;

    interSharedPtr^.interFile^.block[nextintcode].s := s;
    putintfile;
    end
end;
{>>>}

{<<<}
procedure verify (set1: tokenset; {acceptable tokens}
                 set2: tokenset; {tokens to finish skip}
                 err: warning {Message if token not ok} );
{ Check that the current token is in set1, emit an error message
  "err" if not, and skip until the token is in set1 + set2 + neverskipset.
  The error message will be placed as close to the estimated location of the token as possible
  }
var
  skipset: tokenset; {Tokens which end skipping}

begin
  if not (token in set1) then
    begin
    skipset := set1 + set2 + neverskipset;
    if token in skipset then warnbetween(err)
    else
      begin
      warn (err);
      while not (token in skipset) do
        gettoken
      end
    end
end;
{>>>}
{<<<}
procedure verify1 (set1: tokenset; {acceptable tokens} err: warning {message if token not ok} );
{ Same as verify, except no separate skip set is provided }

begin
  verify(set1, [], err);
end;
{>>>}
{<<<}
procedure verifytoken (tok: tokentype; {acceptable token}
                      err: warning {message if token not ok} );
{ Check for a given token (tok) and skip it if found.  If not
  found, emit an error message set by "err".  This is used for
  redundant tokens in the syntax, where parsing can continue if it is missing. }

begin
  if token = tok then
    gettoken
  else
    warnbetween (err)
end;
{>>>}

{<<<}
procedure enterform (newtyp: types; {type for this form}
                     var where: tableIndex; {new entry}
                     var whereptr: entryptr {for access to new entry} );
{ Enter a new formentry at the current level
  This also gets a formnumber for use with the debugger, and sets the type to be newtyp
}
begin
  if tabletop = tablesize then
    analysFatal (tablefull)
  else
    tabletop := tabletop + 1;

  where := tabletop;
  whereptr := ref(bigtable[tabletop]);
  with whereptr^ do
    begin
    if (newtyp in [subranges, scalars, fields, arrays, sets, files, ptrs,
                   ints, bools, chars, reals, doubles, conformantarrays, strings]) then
      begin
      lastdebugrecord := lastdebugrecord + 1;
      dbgsymbol := lastdebugrecord;
      end
    else
      dbgsymbol := 0;

    form := true;
    typ := newtyp;
    containsfile := false;
    packedflag := false;
    bitaddress := false;
    extendedrange := false;
    disposable := false;
    end;
end;
{>>>}
{<<<}
function getform (objecttype: entryptr {desired form} ): types;
{ Get the basic form associated with a type }

begin
  with objecttype^ do
    if typ = subranges then
      getform := parentform
    else
      getform := typ;
end;
{>>>}

{<<<}
procedure searchlsection (value1: integer; {label value}
                         labellist: labelptr; {root of label list}
                         var wherefound: labelptr {result of search} );
{ Search a list of labels starting at "labellist" for a label with the
  value "value".  The result is returned in "wherefound".  If the label
  is not in the list, the returned entry will be "labelflag".
  "Labelflag" is set to the desired value to simplify the search algorithm. }

begin {searchlsection}
  labelflag^.labelvalue := value1;
  wherefound := labellist;
  while wherefound^.labelvalue <> value1 do
    wherefound := wherefound^.nextlabel;
end {searchlsection} ;
{>>>}
{<<<}
procedure searchlabels (value1: integer; {label value}
                       var wherefound: labelptr {result of search} );
{ Search all available scopes for a label with "value", returning the
  result in "wherefound."  The result will be set to "labelflag" if the label cannot be found. }

var
  i: levelindex; {induction var for level search}

begin {searchlabels}
  i := level;
  repeat
    searchlsection(value1, display[i].labellist, wherefound);
    i := i - 1;
  until (i = 0) or (wherefound <> labelflag);
  lev := i + 1;
end {searchlabels} ;
{>>>}
{<<<}
procedure searchvariants (var currentrecord: tableIndex; {record to search}
                         labvalue: operand {varnt label value} );
{ Search a record variant part starting at "currentrecord" for a
  variant with a label of "labvalue" and set "currentrecord" to that
  variant.  If there is no variant with the desired label,
  "currentrecord" is unmodified, and an error message is emitted. }

var
  t: tableIndex; {used to trace variant chain}
  t1: tableIndex; {used to trace label chain}
  t2: tableIndex; {holds last value of t for later use}
  ptr, ptr1: entryptr; {used to access variant and label chains}
  found: boolean; {set if label found, controls search}

begin {searchvariants}
  found := false;
  ptr := ref(bigtable[currentrecord]);
  t := ptr^.firstvariant;
  while (t <> 0) and not found do
    begin
    ptr := ref(bigtable[t]);
    with ptr^ do
      begin
      t2 := t;
      t := nextvariant;
      t1 := firstlabel;
      while (t1 <> 0) and not found do
        begin
        ptr1 := ref(bigtable[t1]);
        with ptr1^ do
          begin
          found := (labvalue.cvalue.intvalue = varlabvalue);
          t1 := nextvarlab;
          end;
        end;
      end;
    end;
  if found then currentrecord := t2
  else warnbefore(badtagerr);
end {searchvariants} ;
{>>>}
{<<<}
procedure stripsubrange (var objectindex: tableIndex {form to be stripped} );
{ Convert a subrange type to the base type for use in an expression. }

var
  ptr: entryptr;

begin
  ptr := ref(bigtable[objectindex]);
  with ptr^ do
    if (typ = subranges) then
     objectindex := parenttype;
end;
{>>>}

{<<<}
procedure searchsection (id: scoperange; {scope id for search}
                         var wherefound: tableIndex {resulting name index} );
{ Search the symbol table for the current token identifier in scope
  "id".  If found, the name index will be placed in "wherefound".  If not
  found, zero will be returned. If the identifier found proves to be
  a constant or type identifier we will update "lastoccurrence" within
  the symbol table entry to allow enforcement of scope restrictions. }

var
  p: entryptr; {used for name table access}
  twherefound: tableIndex; {temp for wherefound during procedure}

begin {searchsection}
  twherefound := keymap[thistoken.key];
  p := ref(bigtable[twherefound]);
  while (twherefound <> 0) and (p^.name <> id) do
    begin
    twherefound := p^.nextname;
    p := ref(bigtable[twherefound]);
    end;
  if not probing and (twherefound <> 0) and
     (p^.lastoccurrence < display[displaytop].scopeid) and
     (p^.namekind in
     [procname, funcname, constname, typename, standardproc, standardfunc, undeftypename, fieldname, undefname]) then
    begin
    p^.lastoccurrence := display[displaytop].scopeid;
    end;
  wherefound := twherefound;
end {searchsection} ;
{>>>}
{<<<}
procedure search (var wherefound: tableIndex {result of search} );
{ Search all available scopes for the current token.  The result is
  returned in "wherefound", with zero indicating no find.  The global
  variable "lev" is set to the level where the token was found. }

var
  i: levelindex; {induction var for level search}
  t: tableIndex; {temp result of search for each level}

begin {search}
  i := displaytop + 1;
  repeat
    i := i - 1;
    searchsection(display[i].blockid, t);
  until (i = 0) or (t <> 0);
  lev := i;
  wherefound := t;
end {search} ;
{>>>}

{<<<}
function lower (f: entryptr {form to check} ): integer;
{ Returns the lower bound of "f".  This is meaningful only for scalar types. }

begin
  with f^ do
    if typ = ints then
      lower := sharedPtr^.targetminint
    else if typ = subranges then
      lower := lowerord
    else
      lower := 0;
end;
{>>>}
{<<<}
function upper (f: entryptr {form to check} ): integer;
{ Returns the upper bound of "f".  This is meaningful only for scalar types }

begin
  with f^ do
    case typ of
      ints: upper := sharedPtr^.targetmaxint;
      bools: upper := 1;
      chars: upper := charsetsize - 1;
      none: upper := 255;
      scalars: upper := lastord;
      subranges: upper := upperord;
      otherwise upper := sharedPtr^.targetmaxint
      end
end;
{>>>}
{<<<}
function bits (i: integer {value to find size of} ): integer;
{ Returns the number of bits needed to contain the value of i }

var
  b: integer; {Accumulates number of bits}
  value: unsignedint; {Temp so can use a register and shift inst}

begin
  if i < 0 then bits := sharedPtr^.targetintsize * bitsperunit
  else
    begin
    value := i;
    b := 1;
    while value > 1 do
      begin
      b := b + 1;
      value := value div 2;
      end;
    bits := b;
    end;
end;
{>>>}
{<<<}
function sizeof (f: entryptr; {Form to get size of}
                 packedresult: boolean {set if packed value} ): addressrange;
{ Returns the amount of storage needed to contain a value of the type
  specified by "f".  If "packedresult" is set, this is in bits, otherwise it is in addressing units }

var
  lowerf: integer; { temp holding lower(f) }
  magnitude: addressrange; {absolute value of max number of bits}

begin
  if packedresult = f^.bitaddress then
    sizeof := f^.size

  else if packedresult then
    case f^.typ of
      chars, bools, scalars, subranges, none:
        begin
        lowerf := lower(f);
        if (lowerf < 0) then
          begin
          magnitude := max(abs(upper(f)), abs(lowerf + 1));
          if magnitude = 0 then
            sizeof := 1 {handles the case of -1..0}
          else
            sizeof := bits(magnitude) + 1; {the normal case}
          end
        else
          sizeof := bits(upper(f));
        end
      otherwise
        if maxaddr div bitsperunit < f^.size then
          sizeof := maxaddr
        else
          sizeof := f^.size * bitsperunit;
      end
  else
    sizeof := (f^.size + bitsperunit - 1) div bitsperunit;
end;
{>>>}
{<<<}
function forcealign (size: addressrange; {value to align}
                    alignment: addressrange; {requirement}
                    packedresult: boolean {size is in bits} ): addressrange;
{ Forces "size" to the next higher multiple of "alignment".
  Used to overcome limitations built into much contemporary hardware }

begin
  if packedresult then
    alignment := alignment * bitsperunit;
  if alignment > 1 then
    size := ((size + alignment - 1) div alignment) * alignment;
  forcealign := size;
end;
{>>>}
{<<<}
function unsigned (f: entryptr; {type to check}
                  len: addressrange; {space actually allocated for var}
                  packedelement: boolean {set if packed var} ): boolean;
{ Returns true if the values of type "f" are unsigned.
  If "len" is not equal to the space required for the value, it is being
  allocated a space larger than required, and should be treated as signed
  or unsigned for unpacking, depending on the global "unsignedprefered"
}

begin
  if not packedelement then
    len := len * bitsperunit;
  unsigned := not (f^.typ in [subranges, ints, bools, chars, scalars]) or
              (lower(f) >= 0) and (unsignedprefered or (len = sizeof(f,
              true))) or f^.extendedrange;
end;
{>>>}
{<<<}
function simplesize (i: integer {value to find size of} ): integer;
{ Returns the size in multiples of addressing units needed to contain the value of i. }

var
  b: integer; {bits to contain i}
  t: integer; {used to accumulate size in units}

begin
  b := bits(i);
  t := 1;
  while b > t * bitsperunit do
    t := t + 1;
  simplesize := t;
end;
{>>>}

{<<<}
function negaterealconst (realbuffer: realarray {real constant value} ): realarray;
 { function to negate a real constant independent of the host }

const
  halfword = 32768; {for constant negating}

begin
  if realbuffer[1] >= halfword then
    realbuffer[1] := realbuffer[1] - halfword
  else
    realbuffer[1] := realbuffer[1] + halfword;

  negaterealconst := realbuffer;
end;
{>>>}
{<<<}
procedure constant (follow: tokenset; {legal following symbols}
                   dumpvalue: boolean; {true says dump string}
                   var value1: operand {resulting constant} );
{ Syntactic routine to parse a constant.
  productions:
  constant = [ sign ] (unsigned-number | constant-identifier) |
         character-string | structured-constant  .
  If the constant is a simple constant, the actual value is returned in
  "value", otherwise a pointer to the string file is returned.  Constant
  structures are added to the "consttable" portion of the string file
  if they are longer than an integer value, otherwise they are returned
  as an integer value.
}
  var
    negate: boolean; {set if neg sign found}
    sign: boolean; {set if any sign found}
    t: tableIndex; {temp index for constant identifier}
    p: entryptr; {temp access to constant identifier entry}
    t1: entryptr; {Temp for index type for string constant}
    unsvalue: unsignedint; {temp for unsigned operation}

  begin
    { init descriptor }
    with value1, cvalue do
      begin
      typeindex := noneindex;
      operandkind := constoperand;
      representation := ints; {the most common case}
      negated := false;
      intvalue := 0; {this field will be used, even if it is a bad constant}
      extended := false;
      end;

    negate := (token = minus);
    sign := negate or (token = plus);
    if sign then
      gettoken;
    verify (begunsignedset, follow, badconsterr);

    if token in begunsignedset then
      begin
      if token = nilsym then
        begin
        value1 := nilvalue;
        gettoken
        end
      else if token = ident then
        {<<<  either constant or constant structure}
        begin
        structtype := noneindex;
        search(t);
        p := ref(bigtable[t]);
        if t = 0 then
          begin
          warn (undefidenterr);
          gettoken;
          end
        else
          with p^ do
            if namekind = constname then
              begin
              value1.typeindex := consttype;
              t1 := ref(bigtable[consttype]);
              value1.oprndlen := sizeof(t1, false);
              value1.cvalue := constvalue;
              gettoken;
              end
            else if (namekind = typename) then
              begin
              { Note:  this is a kluge to get structconst into an overlay}
              structtype := p^.typeindex;
              structfollow := follow;
              conststructure (structfollow, structtype, structvalue);
              value1 := structvalue;
              { Note: end of kluge, normally a simple call would suffice}
              end
            else
              begin
              warn(badconsterr);
              gettoken;
              end;
        end
        {>>>}
      else {not nil or an identifier}
        with value1, cvalue do
          begin
          case token of
            {<<<}
            intconst:
              begin
              typeindex := intindex;
              intvalue := thistoken.intvalue;
              negated := false; {is default}
              oprndlen := sharedPtr^.targetintsize;
              end;
            {>>>}
            {<<<}
            realconst:
              begin
              typeindex := realindex;
              representation := reals;
              realvalue.realbuffer := thistoken.realvalue;
              oprndlen := sharedPtr^.targetrealsize;
              end;
            {>>>}
            {<<<}
            dblrealconst:
              begin
              typeindex := doubleindex;
              representation := doubles;
              realvalue.realbuffer := thistoken.realvalue;
              oprndlen := doublesize;
              end;
            {>>>}
            {<<<}
            charconst:
              begin
              typeindex := chartypeindex;
              intvalue := thistoken.intvalue;
              negated := false; {is default!}
              oprndlen := charsize;
              end;
            {>>>}
            {<<<}
            stringconst:
              begin { Must make type entry for the string}
              representation := arrays;
              stringconstflag := true;
              len := thistoken.len;
              if (thistoken.pos < 0) and dumpvalue then
                begin
                pos := sharedPtr^.stringfilecount + 1;
                dumpstr (len + 1, sharedPtr^.curstringbuf, true);
                end
              else
                pos := thistoken.pos;

              enterform (subranges, t, t1);
              with t1^ do
                begin
                size := sharedPtr^.targetintsize;
                align := intalign;
                parenttype := intindex;
                parentform := ints;
                lowerord := 1;
                upperord := len
                end;
              enterform (arrays, typeindex, t1);

              with t1^ do
                begin
                packedflag := true;
                bitaddress := true;
                containsfile := false;
                elementtype := chartypeindex;
                stringtype := true;
                arraymembers := len;
                indextype := t;
                size := len div (bitsperunit div stringeltsize);
                if len mod (bitsperunit div stringeltsize) <> 0 then
                  size := size + 1;
                oprndlen := size;
                size := size * bitsperunit;
                elementsize := stringeltsize;
                align := stringalign;
                end;
              end
            {>>>}
            end;
          gettoken;
          end;

      with value1, cvalue do
        if sign and (typeindex <> realindex) and (typeindex <> intindex) then
          warn (badconsterr)
        else if negate then
          if typeindex = realindex then
            realvalue.realbuffer := negaterealconst (realvalue.realbuffer)
          else {non real}
            begin
            if intvalue <> - sharedPtr^.targetmaxint - 1 then
              intvalue := - intvalue;
            negated := not negated;
            end;
      end;

    with value1, cvalue do
      if representation = ints then
        begin
        unsvalue := intvalue;
        extended := (unsvalue > sharedPtr^.targetmaxint) and not negated;
        end;
  end;
{>>>}
{<<<}
function identical (left, right: tableIndex): boolean;
{ True if two types are identical, or if either is undefined (to avoid redundant messages) }

begin
  identical := (left = right) or (left = noneindex) or (right = noneindex);
end;
{>>>}
{<<<}
function compatible (left, right: tableIndex): boolean;
{ True if the types represented by the two input forms are compatible as defined by the Pascal standard.
  If either input is undefined, they are assumed to be compatible to eliminate redundant error messages
}
var
  lptr, rptr: entryptr; { used for access to symbol table }
  c: boolean; {temporary value of compatible}

begin
  stripsubrange (left);
  stripsubrange (right);

  if identical (left, right) then
    compatible := true
  else
    begin
    compatible := false;
    lptr := ref(bigtable[left]);
    rptr := ref(bigtable[right]);
    if lptr^.typ = rptr^.typ then
      case lptr^.typ of
        strings: compatible := true;
        arrays:
          compatible := lptr^.stringtype and rptr^.stringtype and (lptr^.arraymembers = rptr^.arraymembers);
        sets:
          compatible := compatible(lptr^.basetype, rptr^.basetype) and
                        ((lptr^.packedflag = rptr^.packedflag) or lptr^.constructedset or rptr^.constructedset);
        ptrs:
          if (left = nilindex) or (right = nilindex) then
            compatible := true
          else
            begin {Allow compatibility between pointer types and pointers
                   created with the address operator if the base types are the same.
                   Also forstall error messages if either pointer base type is undef. }
            lptr := ref(bigtable[lptr^.ptrtypename]);
            rptr := ref(bigtable[rptr^.ptrtypename]);

            c := (lptr^.typeindex = noneindex) or (rptr^.typeindex = noneindex);
            if rptr^.refdefined or lptr^.refdefined then
              c := c or compatible(rptr^.typeindex, lptr^.typeindex);
            compatible := c;
            end;
        end;
    end;
end;
{>>>}
{<<<}
function alignmentof (f: entryptr; packedresult: boolean): alignmentrange;
{ Compute the alignment requirement of a type.  This function is needed
  strictly because the alignment of a subrange is kluged to the parent
  type to give better code generation on certain types of machines.  This
  kluge causes trouble with packed types, so is deleted if the result
  is to be used in a packed structure. }

begin
  if packedresult = f^.bitaddress then
    alignmentof := f^.align
  else if packedresult then
    alignmentof := f^.align * bitsperunit
  else
    alignmentof := (f^.align + bitsperunit - 1) div bitsperunit;
end;
{>>>}

{<<<}
procedure alloc (align: alignmentrange; {variable alignment}
                 length: addressrange; {length of variable}
                 var spacesize: addressrange; {size of data space}
                 var varloc: addressrange; {loc of new variable}
                 var overflowed: boolean {true if size overflowed} );
{ Allocate space for a single unpacked variable or field.
  A single field of length "length", with alignment "align",
  is added to the end of a data space which already had
  "spacesize" addressing units allocated.  The address of the
  newly allocated field is returned in "varloc", and "spacesize"
  is updated to include the new field.
  ***M68000***
  The variable is simply allocated at the proper alignment.
}
begin
  spacesize := forcealign (spacesize, align, false);
  varloc := spacesize;
  if maxaddr - spacesize > length then
    begin
    spacesize := spacesize + length;
    overflowed := false;
    end
  else overflowed := true;
end;
{>>>}
{<<<}
procedure allocpacked (align: alignmentrange; {variable alignment}
                       length: addressrange; {length of variable}
                       var spacesize: addressrange; {size of data space}
                       var varloc: addressrange; {loc of new variable}
                       var overflowed: boolean; {true if size overflowed}
                       var unusedspace: boolean {space skipped} );
{ Allocate space for a single packed field.
  A single field of length "length", with alignment "align",
  is added to the end of a data space which already had
  "spacesize" addressing units allocated.  The address of the
  newly allocated field is returned in "varloc", and "spacesize"
  is updated to include the new field.  "Unusedspace" is set
  if some space was left due to the allocation strategy.  This
  can be used to modify earlier fields for better access if desired.
  ***M68000***
  Alignment is normally done bitwise, except when the resulting
  field will cross a word boundary, in which case it is advanced
  to the next boundary.  The alignment field is used only if the
  field will cross a byte boundary but not a bit boundary.  This
  is to make sure that no element of a packed array will split a byte boundary
}

var
  newspace: addressrange; {updated value of spacesize}

begin
  if spacesize mod bitsperunit + length > bitsperunit then
    if spacesize mod (packingunit * bitsperunit) + length >
       (packingunit * bitsperunit) then
      newspace := forcealign(spacesize, packingunit * bitsperunit, false)
    else newspace := forcealign(spacesize, align, false)
  else newspace := spacesize;
  unusedspace := newspace > spacesize;
  varloc := newspace;
  if maxaddr - newspace > length then
    begin
    spacesize := newspace + length;
    overflowed := false;
    end
  else
    overflowed := true;
end;
{>>>}
{<<<}
procedure getallocdata (form: entryptr; {type being allocated}
                        varkind: nametype; {type of field or var}
                        packedresult: boolean; {result goes in packed field}
                        spacelen: addressrange; {space consumed so far}
                        var fieldlen: addressrange; {size to allocate}
                        var fieldalign: alignmentrange; {alignment for alloc}
                        var maxalign: alignmentrange {max alignment so far} );
{ Extract allocation data for a field or variable.
  This routine determines the length and alignment requirements for a
  field, variable, or parameter.  This may not be exactly the same
  as the space required for the value, since the field may be accessed
  indirectly, or other limitations may apply.
  "Form" points to the type entry for this variable, and "varkind" gives
  the usage.  "Fieldlen" and "fieldalign" must be set to define the space
  to be allocated, and "maxalign" must be updated to the alignment for
  the entire data space.  "Packedresult" and "spacelen" are included for
  information.
  ***M68000***
  Variable parameters are all assigned space for pointers to the variable.
  Other parameters are aligned to a word boundary, since the stack is always
  pushed in increments of a word.  Other variables are allocated the space
  and alignment required by the type.  If a packed field crosses a byte
  boundary, then the entire structure must be aligned on a word boundary,
  since the allocation scheme is dependent on word access }

begin
  if varkind in [varparam, confparam, varconfparam] then
    begin
    fieldlen := sharedPtr^.ptrsize;
    fieldalign := ptralign;
    end
  else
    begin
    fieldlen := sizeof(form, packedresult);
    if (varkind = param) or (varkind = boundid) then
      fieldalign := stackalign
    else if packedresult and not (form^.typ in [arrays, fields]) and
            (spacelen mod (packingunit * bitsperunit) + fieldlen <=
            packingunit * bitsperunit) then
      fieldalign := 1
    else fieldalign := alignmentof(form, packedresult);
    if packedresult and (fieldlen <= packingunit * bitsperunit) and
       (spacelen mod bitsperunit + fieldlen > bitsperunit) then
      maxalign := max(maxalign, packingunit * bitsperunit);

      {The following fix corrects alignment problems with non-packed items but
       it is incompatible with old structures, so it is the subject of much debate.
      else }
      { Align non-packed items longer than a byte on a word boundary. }
      { if not packedresult and (fieldlen >= 2) then
      fieldalign := max(fieldalign, 2);}
    end;

  maxalign := max(maxalign, fieldalign);
end;
{>>>}

{<<<}
function arraysizeof (f: entryptr; {form to get size of}
                      packedresult: boolean {set if packed } ): addressrange;
{ Returns the amount of storage needed to contain a value of the type
  specified by "f", with desired modifications to ease array accessing.
  "Packedresult" determines if the size is in bits or addressing units.
  ***M68000***
  If the result is packed, and the size is greater than an addressing
  unit, the result is rounded up to an even unit.  If the size is not
  greater than an addressing unit, it is rounded up to an even power of
  two bits.
}
var
  s: addressrange; {working value of size}
  s1: addressrange; {used to compute power of two size}

begin
  s := sizeof (f, packedresult);
  if packedresult then
    if s > bitsperunit then
      s := sizeof(f, false) * bitsperunit
    else
      begin
      s1 := 1;
      while s > s1 do
        s1 := s1 * 2;
      s := s1;
      end
  else
    s := forcealign(s, alignmentof(f, false), false);

  if s <> 0 then
    arraysizeof := s
  else
    arraysizeof := 1;
end;
{>>>}
{<<<}
procedure packedstore (eltloc: addressrange; {rel address of this field}
                       eltsize: addressrange; {size of this field}
                       baseloc: addressrange; {rel address of first buffer}
                       val: integer; {value to pack}
                       var pbuf1, pbuf2: integer; {packed buffers}
                       var full: boolean {buffer 1 is full} );
{ This routine stores the value "val" as a packed field.  The field address,
  in bits is "eltloc".  The result is to be returned in two integer buffers
  "pbuf1" and "pbuf2", which should be otherwise undisturbed.  The boolean
  "full" is set when "pbuf1" is full, and should be written.  "Baseloc" is
  the address (in bits) of the start of "pbuf1"
}

var
  mask: unsignedint; {masks off unwanted bits of "val"}
  i: addressrange; {induction variable}
  shiftdist: addressrange; {distance to shift value}

begin
  if emitflag then {after errors, just stop}
    begin
    eltsize := min(eltsize, hostintsize * bitsperunit); { Prevents big
      loops! }
    mask := 1;
    for i := 2 to eltsize do mask := mask * 2 + 1;
    val := val and mask;
    shiftdist := eltloc - baseloc;
    full := false;
    while shiftdist >= hostintsize * bitsperunit do
      begin
      full := true;
      shiftdist := shiftdist - hostintsize * bitsperunit;
      end;

    for i := 1 to hostintsize * bitsperunit - shiftdist - eltsize do
      val := val * 2;
    if full then
      pbuf2 := pbuf2 + val
    else
      pbuf1 := pbuf1 + val;
    end;
end;
{>>>}
{<<<}
function roundpackedsize (spacesize: addressrange; {rounded space}
                          packedresult: boolean {true if packed record} ): addressrange;
{Round length of declared type to an even multiple of bitsperunit if
 the type is packed.  Used to simplify code generator, which wishes to
 use unit-move instructions for structure assignments.
}
begin
  if packedresult and (spacesize > packingunit * bitsperunit) then
    roundpackedsize := forcealign(spacesize, unitsize, true)
  else
    roundpackedsize := spacesize;
end;
{>>>}
{<<<}
procedure possibletemp (off: addressrange; vartype: tableIndex; debugrec: integer);
{
    Purpose:
      Determine if tha var at off is eligible for assignment to register
    Inputs:
      off: offset from local data area of the possible temp.
      vartype: symbol table index to the var's type identifier.
      debugrec: offset in the symbol file where var's allocation is
                described.
    Outputs:
      If conditions are met then file "locals" has record appended.
    Algorithm:
      Straight forward.
    Sideeffects:
      None
    Last Modified: 9/12/86
  }
var
  f: entryptr; { for access to type id }
  localvar: localvartype; { for writing to file }

begin
  if tempvars < maxtrackvar then
    begin
    f := ref(bigtable[vartype]);
    if ((f^.typ = reals) and not sharedPtr^.switcheverplus[doublereals]) or
       (sharedPtr^.switcheverplus[fpc68881] and (f^.typ in [reals, doubles])) or
       ((f^.typ in [bools, chars, ints, ptrs, scalars, subranges]) and
       (f^.size <= sharedPtr^.targetintsize)) then
      begin
      tempvars := tempvars + 1;
      localvar.offset := off;
      localvar.typ := f^.typ;
      localvar.debugrecord := debugrec;
      write (sharedPtr^.localFile, localvar);
      end;
    end;
end;
{>>>}
{>>>}
{<<<  body}
{<<<}
procedure genrealvalue (op: operator; {type of real} r: realarray);
{ Generates a real operand.  This is machine dependent, and must be
  conditionalized, for example, for a crosscompiler
}
var
  kludge: {convert real to integer}
    record
      case integer of
        1: (b: packed array [1..32] of hostfilebyte);
        2: (i: array [1..16] of integer);
        3: (r: realarray);
    end;
  i: 1..32; {induction var for writing}

begin
  kludge.r := r;
  genop (op);
  for i := 1 to size(realarray) div (hostfileunits * hostintsize) do
    genint (kludge.i[i]);
end;
{>>>}
{<<<}
procedure storagelimit (unsigned: boolean; {this is an unsigned value}
                       length: addressrange; {length of field}
                       packedfield: boolean; {a packed field}
                       var limit: integer);
{ Finds the maximum value that the field defined by the arguments will hold This may well be greater than the range }

var
  { these locals are for speed purposes, locals will go to registers
  and the loop will run faster ( sorry iapx..) }
  tlimit: integer; {temp to hold intermediate limit value}
  tlength: addressrange; {temp to hold intermediate length value}

begin
  tlength := length;
  if not packedfield then
    tlength := tlength * bitsperunit;

  { 6/4/85 SFG Found a case where  this ran away on an undeclared
  var. Got real tired of waiting for exit from the while loop!
  new stuff from oregon has yet another way to fix this }
  if not unsigned { and ( tlength <> 0 )} then
    tlength := tlength - 1;

  { this solves looping problem also }
  if tlength > sharedPtr^.targetintsize * bitsperunit then
    tlength := sharedPtr^.targetintsize * bitsperunit;

  tlimit := 0;
  while tlength > 0 do
    begin
    tlimit := tlimit * 2 + 1;
    tlength := tlength - 1;
    end;
  limit := tlimit;
end;
{>>>}

{<<<}
procedure setconstrange (value1: integer; {value to use}
                        negated: boolean; {value was negated}
                        var r: operand_range {resulting range} );

{ Set up a constant operand_range.
}


  begin {setconstrange}
    with r.optimistic do
      begin
      minlimit := value1;
      maxlimit := value1;
      extended := (value1 < 0) and not negated;
      end;
    r.pessimistic := r.optimistic;
  end {setconstrange} ;
{>>>}
{<<<}
procedure settyperange (t: entryptr; var r: range);
{ Set up a range according to the range of t }

begin
  r.minlimit := lower(t);
  r.maxlimit := upper(t);
  r.extended := t^.extendedrange;
end;
{>>>}
{<<<}
procedure setstoragerange (t: entryptr; {variable or field in question}
                          len: addressrange; {length of field}
                          packedfield: boolean; {field is packed}
                          var r: range {resulting range} );
{ Set up a range according to what the storage will hold
}

var
  ov: boolean; {overflow dummy argument}

begin
  with r do
    begin
    ov := unsigned(t, len, packedfield);
    storagelimit(ov, len, packedfield, maxlimit);
    if unsigned(t, len, packedfield) then minlimit := 0
    else minlimit := - maxlimit - 1;
    extended := t^.extendedrange;
    end;
end;
{>>>}
{<<<}
procedure setvarrange (len: addressrange; {range of variable}
                       packedfield: boolean; {this is a packed field}
                       trustrange: boolean {trust the type range data} );
{ Set the range for a variable reference.  If we know that the variable
  contains only legal values, both optimistic and pessimistic estimates
  are set from the type data.  If the variable might be uninitialized,
  the pessimistic estimate is set to the limits which can be contained
  in the storage assigned to it's value.
  The type of the variable is taken from resulttype, and the result is
  placed in oprndstk[sp].value_range
}
begin
  with oprndstk[sp].value_range do
    begin
    settyperange (resultptr, optimistic);
    if trustrange then
      pessimistic := optimistic
    else
      setstoragerange (resultptr, len, packedfield, pessimistic);
    end;
end;
{>>>}
{<<<}
procedure binaryrange (var left, right: operand_range; {arguments}
                       extended: boolean; {an extended range}
                       procedure signedop (left, right: integer; var result: integer; var overflow: boolean);
                       procedure unsignedop (left, right: integer; var result: integer; var overflow: boolean);
                       procedure dorange (left, right: range; var result: range;
                                          procedure op (l, r: integer; var res: integer; var overflow: boolean);
                                          var mayoverflow: boolean);
                       var result: operand_range;
                       var mayoverflow: boolean);
{ Apply the appropriate operation to compute a binary range.
  This is broken out as a rather strange procedure to save a bit
  of space.  The "left" and "right" operands are passed as variable
  parameters solely to save space at the call.
}
var
  ov: boolean; {dummy overflow operation}

begin
  if extended then
    begin
    dorange(left.optimistic, right.optimistic, result.optimistic, unsignedop, mayoverflow);
    dorange(left.pessimistic, right.pessimistic, result.pessimistic, unsignedop, ov);
    end
  else
    begin
    dorange(left.optimistic, right.optimistic, result.optimistic, signedop, mayoverflow);
    dorange(left.pessimistic, right.pessimistic, result.pessimistic, signedop, ov);
    end;
end;
{>>>}
{<<<}
procedure modrange (left, right: range; {operands}
                    var result: range; {result}
                    procedure op(left, right: integer; var result: integer; var overflow: boolean);
                    var mayoverflow: boolean);
{ Compute the resulting range for a "mod" operator.  In most cases, the
  most you can say is that the range is determined by the divisor range.
  If the dividend is constant, we can be a bit more explicit }

var
  ov: boolean; {dummy overflow operator}

begin
  with right do
    mayoverflow := (minlimit = 0) or not extended and (minlimit < 0);
  with left do
    if maxlimit = minlimit then
      begin
      op(maxlimit, right.maxlimit, result.maxlimit, ov);
      result.minlimit := result.maxlimit;
      end
    else
      begin
      result.maxlimit := max(right.maxlimit - 1, 0);
      result.minlimit := 0;
      end;
  result.extended := right.extended;
end;
{>>>}
{<<<}
procedure divrange (left, right: range; {operands}
                    var result: range; {result}
                    procedure op(left, right: integer; var result: integer; var overflow: boolean);
                    var mayoverflow: boolean);
{ Compute the resulting range for a divide operation.  This is
  extremely complicate, as each of the 9 possible sign combinations
  must be considered separately.
}
var
  ov: boolean; {dummy for range divides}
  temp: integer; {temp for negating}
  maxmax, maxmin, minmax, minmin: integer; {cross terms}
  rightpos, rightneg: boolean; {right is always ...}

begin
  mayoverflow := false;
  if (right.minlimit = 0) then
    begin
    right.minlimit := 1;
    mayoverflow := true;
    end
  else if (right.maxlimit = 0) then
    begin
    right.maxlimit := - 1;
    mayoverflow := true;
    end;

  op (left.maxlimit, right.maxlimit, maxmax, ov);
  op (left.maxlimit, right.minlimit, maxmin, ov);
  op (left.minlimit, right.maxlimit, minmax, ov);
  op (left.minlimit, right.minlimit, minmin, ov);

  rightpos := right.extended or (right.minlimit > 0);
  rightneg := not right.extended and (right.maxlimit < 0);
  with left do
    if extended or (minlimit >= 0) then
      begin {dividend always positive}
      if rightpos then
        begin {divisor always positive}
        result.maxlimit := maxmin;
        result.minlimit := minmax;
        end
      else if rightneg then
        begin {divisor always negative}
        result.maxlimit := minmin;
        result.minlimit := maxmax;
        end
      else
        begin {divisor crosses zero}
        result.maxlimit := maxlimit;
        negate(maxlimit, result.minlimit, ov);
        end;
      end
    else if (maxlimit < 0) then
      begin {dividend always negative}
      if rightpos then
        begin {divisor always positive}
        result.maxlimit := maxmax;
        result.minlimit := minmin;
        end
      else if rightneg then
        begin {divisor always negative}
        result.maxlimit := minmax;
        result.minlimit := maxmin;
        mayoverflow := mayoverflow or (minlimit < sharedPtr^.targetminint) and
                       (right.maxlimit = - 1);
        end
      else
        begin {divisor crosses zero}
        negate(minlimit, result.maxlimit, ov);
        result.minlimit := minlimit;
        mayoverflow := true;
        end
      end
    else
      begin {dividend crosses zero}
      if rightpos then
        begin {divisor always positive}
        result.maxlimit := maxmin;
        result.minlimit := minmin;
        end
      else if rightneg then
        begin {divisor always negative}
        result.maxlimit := minmax;
        result.minlimit := maxmax;
        end
      else
        begin {divisor crosses zero}
        negate(minlimit, temp, ov);
        result.maxlimit := max(maxlimit, temp);
        negate(maxlimit, temp, ov);
        result.minlimit := min(temp, minlimit);
        mayoverflow := true;
        end;
      end;
  result.extended := left.extended or right.extended;
end;
{>>>}
{<<<}
procedure addrange (left, right: range; {operands}
                    var result: range; {result}
                    procedure op(left, right: integer; var result: integer; var overflow: boolean);
                    var mayoverflow: boolean);
{ Adjust the range of an operand as a result of an addition.
  If the operation may generate an overflow, the variable "mayoverflow"
  is set.
}
var
  overflow: boolean;

begin
  with result do
    begin
    op(left.minlimit, right.minlimit, minlimit, overflow);
    op(left.maxlimit, right.maxlimit, maxlimit, mayoverflow);
    mayoverflow := mayoverflow or overflow;
    extended := left.extended or right.extended;
    end;
end;
{>>>}
{<<<}
procedure subrange (left, right: range; {operands}
                    var result: range; {result}
                    procedure op(left, right: integer; var result: integer; var overflow: boolean);
                    var mayoverflow: boolean);
{ Adjust the range of an operand as a result of a subtracton.
  If the operation may generate an overflow, the variable "mayoverflow"
  is set.
}
var
  overflow: boolean;

begin
  with result do
    begin
    op(left.minlimit, right.maxlimit, minlimit, overflow);
    op(left.maxlimit, right.minlimit, maxlimit, mayoverflow);
    mayoverflow := mayoverflow or overflow;
    extended := left.extended or right.extended;
    end;
end;
{>>>}
{<<<}
procedure mulrange (left, right: range; {operands}
                    var result: range; {result}
                    procedure op(left, right: integer; var result: integer; var overflow: boolean);
                    var mayoverflow: boolean);
{ Compute the resulting range for an integer multiply.  This is
  a relatively complicated job, since different signs on the
  limits cause different terms to dominate.
}
var
  maxmax, maxmin, minmax, minmin: integer; {temp results}
  ov: boolean; {operation overflowed}

begin
  op (left.maxlimit, right.maxlimit, maxmax, mayoverflow);
  op (left.maxlimit, right.minlimit, maxmin, ov);
  mayoverflow := mayoverflow or ov;

  op (left.minlimit, right.maxlimit, minmax, ov);
  mayoverflow := mayoverflow or ov;

  op (left.minlimit, right.minlimit, minmin, ov);
  mayoverflow := mayoverflow or ov;

  if (not left.extended and (left.minlimit <= 0) and (left.maxlimit > 0)) or
     (not right.extended and (right.minlimit <= 0) and
     (right.maxlimit > 0)) then
    begin {one of them crosses zero}
    result.maxlimit := max(maxmax, minmin);
    if (left.extended or right.extended) and mayoverflow then
      result.minlimit := 0
    else result.minlimit := min(maxmin, minmax);
    end

  else if left.extended or (left.minlimit > 0) then
    if right.extended or (right.minlimit > 0) then
      begin
      result.maxlimit := maxmax;
      if (left.extended or right.extended) and mayoverflow then
        result.minlimit := 0
      else result.minlimit := minmin;
      end
    else
      begin {right must be always negative}
      result.maxlimit := minmax;
      if left.extended and mayoverflow then result.minlimit := 0
      else result.minlimit := maxmin;
      end

  else {must be < 0}
    if right.extended or (right.minlimit > 0) then
      begin
      result.maxlimit := maxmin;
      result.minlimit := minmax;
      end
    else
      begin
      result.maxlimit := minmin;
      result.minlimit := maxmax;
      end;

  result.extended := left.extended or right.extended;
end;
{>>>}
{<<<}
procedure andrange (var left, right: range; {operands, var to save space}
                   var result: range {resulting range} );
{ Compute the resulting range for an integer AND }

var
  minbits: integer; {value with fewest bits in representation}

begin
  if (left.maxlimit = left.minlimit) and
     (right.maxlimit = right.minlimit) then
    begin
    result.maxlimit := left.maxlimit and right.maxlimit;
    result.minlimit := result.maxlimit;
    end
  else if left.extended or right.extended or (left.minlimit >= 0) or (right.minlimit >= 0) then
    begin
    if (left.minlimit < 0) then minbits := right.maxlimit
    else if (right.minlimit < 0) then minbits := left.maxlimit
    else minbits := min(left.maxlimit, right.maxlimit);
    storagelimit(true, bits(minbits), true, result.maxlimit);
    result.minlimit := 0;
    end
  else
    begin
    storagelimit(false, sharedPtr^.targetintsize, false, result.maxlimit);
    result.minlimit := - result.maxlimit - 1;
    end;
  result.extended := left.extended or right.extended;
end;
{>>>}
{<<<}
procedure orrange (var left, right: range; {operands, var to save space}
                  var result: range {resulting range} );
{ Compute the resulting range for an integer OR }

var
  maxbits: integer; {value with most bits in representation}

begin
  if (left.maxlimit = left.minlimit) and
     (right.maxlimit = right.minlimit) then
    begin
    result.maxlimit := left.maxlimit or right.maxlimit;
    result.minlimit := result.maxlimit;
    end
  else if left.extended or right.extended or (left.minlimit >= 0) and (right.minlimit >= 0) then
    begin
    if (left.maxlimit < 0) then maxbits := left.maxlimit
    else if (right.maxlimit < 0) then
      maxbits := right.maxlimit
    else
      maxbits := max(left.maxlimit, right.maxlimit);
    storagelimit (true, bits(maxbits), true, result.maxlimit);
    result.minlimit := 0;
    end
  else
    begin
    storagelimit (false, sharedPtr^.targetintsize, false, result.maxlimit);
    result.minlimit := - result.maxlimit - 1;
    end;

  result.extended := left.extended or right.extended;
end;
{>>>}

{<<<}
procedure checkboolean;
{ Emit an error message if the result type of the current expression is
  not boolean.  Emit no message if the result type is undefined, to avoid redundant messages.
}

begin
  if (resultform <> none) and (resultform <> bools) then
    warnbefore(booleanexpected);
end;
{>>>}
{<<<}
function computecost (i: integer { operand stack index } ): integer;
{ Compute the cost of the operand specified by "i" }

begin
  if oprndstk[i].operandkind = constoperand then
    computecost := 0
  else
    computecost := oprndstk[i].cost;
end;
{>>>}
{<<<}
procedure genlit (i: integer {value to generate} );
{ generate a literal with value "i" }

begin
  genop(lit);
  genint(i)
end;
{>>>}
{<<<}
function constcheck (i: integer {operand stack index} ): boolean;
{ True if operand i is a constant. }

begin
  constcheck := oprndstk[i].operandkind = constoperand
end;
{>>>}

{<<<}
procedure bumpsp;
{ Push a new operand on the stack if there is room. }

begin
  if sp = oprnddepth then
    analysFatal (compilerwritererr);
  sp := sp + 1;
end;
{>>>}

{<<<}
procedure genoprnd;
{ Make sure that the intermediate file outputs needed to access the
  top operand on the stack are generated, then pop the top operand.
  It is necessary to generate output only if the top operand is a
  constant, as the expression parsing routines will have generated
  access code for all other cases.
}
var
  kludge: {convert to bytes or ints}
    record
      case integer of
        1: (b: packed array [0..setvaluebytes] of hostfilebyte);
        2: (s: setvalueblock);
        3: (i: array [0..10] of integer);
    end;
  i: integer; {general use induction var}
  newlim: integer; {new string file limit}
  f: entryptr; {for access to a form entry}
  emptyset: boolean; {true if set const is empty set}
  setcount: integer; {address in string table of set}

begin {genoprnd}
  if sp = -1 then
    analysFatal (compilerwritererr);
  with oprndstk[sp] do
    if operandkind = constoperand then
      with cvalue do
        case representation of
          ptrs:
            begin
            genop(ptrop);
            genint(niladdressvalue);
            end;
          ints, chars, bools, scalars:
            begin
            genop(intop);
            genint(intvalue);
            end;
          reals: genrealvalue(realop, realvalue.realbuffer);
          doubles: genrealvalue(doubleop, realvalue.realbuffer);
          strings, arrays, fields:
            begin
            f := ref(bigtable[typeindex]);
            if f^.disposable and (typeindex = tabletop) then
              begin
              lastdebugrecord := lastdebugrecord - 2;
              tabletop := tabletop - 2;
              end;
            genop(structop);
            genint(pos);
            genint(oprndlen);
            end;
          sets:
            begin
            emptyset := setvalue^ = [];
            if emptyset and emptysetgenerated then setcount := emptysetcount
            else
              begin
              newlim := forcealign (sharedPtr^.stringfilecount, setalign * hostfileunits, false);
              while newlim > sharedPtr^.stringfilecount do
                putbyte (0);
              setcount := sharedPtr^.stringfilecount;

              if emptyset then
                begin
                emptysetgenerated := true;
                emptysetcount := setcount;
                for i := 0 to setvaluebytes do
                  putbyte (0);
                end
              else
                begin
                kludge.s := setvalue^;
                for i := 0 to oprndlen * hostfileunits - 1 do
                  putbyte (kludge.b[i]);
                end;
              end;

            genop(structop);
            genint(setcount);
            genint(oprndlen);
            dispose(setvalue);
            end;
          otherwise; {in case of syntax errors}
          end;
  sp := sp - 1;
end {genoprnd} ;
{>>>}
{<<<}
procedure debugstmt (s: stmttype; line: integer; filepos: integer; fileIndex: integer);

begin
  genstmt(s);

  { Only need to do sourcestringindex once.  If source name changes,
    and somebody sets to nonzero value, it'll be put out again }
  if sourcestringindex <> fileIndex then
    begin
    sourcestringindex := fileIndex;
    genint (fileIndex);
    end
  else
    genint(0);

  genint(line);
end;
{>>>}
{<<<}
procedure newexprstmt (s: stmttype { statement to generate } );
{ Begin a new statement which has an expression as part of its structure }

begin
  debugstmt (s, thistoken.line, thistoken.filepos, thistoken.fileIndex);
  intstate := opstate;
end;
{>>>}
{<<<}
procedure getexprstmt (s: stmttype { statement starting } );
{ Begin a new statement with expression and get the next token.
}
begin {getexprstmt}
  newexprstmt(s);
  gettoken;
end {getexprstmt} ;
{>>>}
{<<<}
procedure genoprndstmt;
{ Terminate the expression(s) being compiled and prepare for the next statement }

begin {genoprndstmt}
  genoprnd;
  genop(endexpr);
  intstate := stmtstate;
end {genoprndstmt} ;
{>>>}
{<<<}
procedure pushint (i: integer {value to push} );
{ Push an integer constant onto the operand stack.
}
begin {pushint}
  bumpsp;
  with oprndstk[sp] do
    begin
    typeindex := intindex;
    oprndlen := sharedPtr^.targetintsize;
    operandkind := constoperand;
    cvalue.representation := ints;
    cvalue.intvalue := i;
    cvalue.negated := false;
    extended := false;
    setconstrange(i, (i < 0), value_range);
    end
end {pushint} ;
{>>>}
{<<<}
procedure pushdummy;
{ Push a dummy operand on the stack.  This is strictly a place holder,
  and will generate no intermediate file output.
}
begin {pushdummy}
  pushint(0);
  oprndstk[sp].operandkind := exproperand;
  oprndstk[sp].cost := 0;
end {pushdummy} ;
{>>>}

{<<<}
procedure checkrange (subject: operand;         { does this fit? }
                      believeit: boolean;       { if true, use optimistic estimate }
                      var outofbounds: boolean; { definitely out of bounds }
                      var lowermaybe: boolean;  { lower range needs a check }
                      var uppermaybe: boolean   { upper range needs a check });
{ Check the range associated with "subject" and see how it fits with the type.
  The boolean variables are used to determine runtime range checking.
  The "believeit" flag determines how much we believe the range data.
}
var
  r: range; {range to check against}
  usmin, usmax: unsignedint; {unsigned values for check}
  l, u: integer; {lower and upper type limits}
  typeptr: entryptr; {for access to typeindex data}

begin
  with subject, value_range do
    begin
    if believeit then
      r := optimistic
    else
      r := pessimistic;

    typeptr := ref(bigtable[typeindex]);
    l := lower(typeptr);
    u := upper(typeptr);

    if typeptr^.extendedrange or r.extended then
      begin
      usmin := optimistic.minlimit;
      usmax := optimistic.maxlimit;
      outofbounds := (usmax < l) or (usmin > u);
      usmin := r.minlimit;
      usmax := r.maxlimit;
      lowermaybe := usmin < l;
      uppermaybe := usmax > u;
      end
    else
      begin
      outofbounds := (optimistic.maxlimit < l) or (optimistic.minlimit > u);
      lowermaybe := r.minlimit < l;
      uppermaybe := r.maxlimit > u;
      end;
    end;
end;
{>>>}
{<<<}
procedure newresulttype (newtype: tableIndex);
{ Set the value of resulttype to newtype and make it available in newptr.
}
begin
  resulttype := newtype;
  resultptr := ref(bigtable[resulttype]);
  resultform := getform(resultptr);
end;
{>>>}
{<<<}
procedure pushconstant (follow: tokenset);
{ Parse and push a constant operand onto the stack.
}
begin {pushconstant}
  bumpsp;
  constant(follow, true, oprndstk[sp]);
  newresulttype(oprndstk[sp].typeindex);
  with oprndstk[sp], cvalue do
    if representation = ints then
      setconstrange(intvalue, negated, value_range);
end {pushconstant} ;
{>>>}
{<<<}
procedure newstringtype (var newtype: tableIndex; {returns index of new string type}
                        newform: types; {arrays or strings}
                        len: addressrange {number of chars in string} );
{ Build a new type describing a string.  This can be either a standard
  "packed array [1..len] of char" string or extended "string[len]".
  Note from above description that "len" refers to data bytes, not including
  the length byte for an extended string, to be allocated.
}
var
  t: tableIndex; {holds subrange}
  t1: entryptr; {for filling in table entry}

begin
  enterform (subranges, t, t1);
  with t1^ do
    begin
    size := sharedPtr^.targetintsize;
    align := intalign;
    parenttype := intindex;
    parentform := ints;
    if newform = arrays then lowerord := 1
    else
      begin
      lowerord := 0;
      len := len + 1;
      upperord := len;
      end;
    end;

  enterform (newform, newtype, t1);
  with t1^ do
    begin
    packedflag := true;
    bitaddress := true;
    containsfile := false;
    disposable := true;
    elementtype := chartypeindex;
    stringtype := newform = arrays;
    arraymembers := len;
    indextype := t;

    size := len div (bitsperunit div stringeltsize);
    if len mod (bitsperunit div stringeltsize) <> 0 then
      size := size + 1;
    size := size * bitsperunit;

    elementsize := stringeltsize;
    align := stringalign;
    end;
end;
{>>>}
{<<<}
function range_length (r: range {return length of this range in bytes} ): addressrange;
{ Computes the number of bytes required to compute result in the given range.
  This is machine dependent as in most cases we don't want to compute lengths not supported by the machine.
}
var
  l: addressrange;
  result: addressrange; {hold the result}

begin
  with r do
    if extended then
      result := sharedPtr^.targetintsize
    else
      begin
      l := bits(max(abs(minlimit + 1), abs(maxlimit)));
      l := l + 1;
      {allow for sign bit and signed 16-bit relative mode on 68K}
      if l <= 16 then
        result := 2
      else
        result := 4;
      end;

  range_length := min(result, sharedPtr^.targetintsize);
end;
{>>>}

{<<<}
function getintvalue (i: integer {operand stack index} ): integer;
{ Return the integer value of operand i, which is assumed to
  be an integer constant.
}
begin {getintvalue}
  if oprndstk[i].cvalue.representation = ints then
    getintvalue := oprndstk[i].cvalue.intvalue
  else getintvalue := 1;
end {getintvalue} ;
{>>>}
{<<<}
function getrealvalue (i: integer {operand stack index} ): real;
{ Return the real value of operand i, which is assumed to be a
  real constant in host format.
}
begin {getrealvalue}
  if (oprndstk[i].cvalue.representation = reals) or
     (oprndstk[i].cvalue.representation = doubles) then
    getrealvalue := oprndstk[i].cvalue.realvalue.realbinary
  else getrealvalue := 1.0;
end {getrealvalue} ;
{>>>}
{<<<}
function getrealbuffer (i: integer {operand stack index} ): realarray;
{ Return the real value of operand i, which is assumed to be a real constant in any supported format format }

begin {getrealbuffer}
  if (oprndstk[i].cvalue.representation = reals) or
     (oprndstk[i].cvalue.representation = doubles) then
    getrealbuffer := oprndstk[i].cvalue.realvalue.realbuffer
  else
    begin
    getrealbuffer[1] := 1;
    getrealbuffer[2] := 1;
    getrealbuffer[3] := 1;
    getrealbuffer[4] := 1;
    end;
end {getrealbuffer} ;
{>>>}

{<<<}
procedure power2check (n: integer; { number to check }
                      var power2: boolean; {true if n = power of 2}
                      var power2value: integer {resulting power} );
{ Find out if n is an even power of 2, and return the exponent if so.
  ****self hosted version
}
begin {power2check}
  power2value := 0;
  while (n > 0) and not odd(n) do
    begin
    n := n div 2;
    power2value := power2value + 1;
    end;
  power2 := (n = 1);
end {power2check} ;
{>>>}
{<<<}
procedure foldcommon;
{ Common part of binary folding routines.  Pops the stack and sets isconst
  for the returned value.
}
begin {foldcommon}
  sp := sp - 1;
  oprndstk[sp].operandkind := constoperand;
end {foldcommon} ;
{>>>}

{<<<}
procedure returnint (intvalue: integer; {value to return}
                    negated: boolean {value has been negated} );
{ Leave an integer constant "intvalue" on the stack in the place of two
  operands.  Used in constant operation folding.
}
begin {returnint}
  foldcommon;
  oprndstk[sp].cvalue.representation := ints;
  oprndstk[sp].cvalue.intvalue := intvalue;
  oprndstk[sp].cvalue.negated := negated;
  oprndstk[sp].extended := (intvalue < 0) and not negated;
  setconstrange(intvalue, negated, oprndstk[sp].value_range);
end {returnint} ;
{>>>}
{<<<}
procedure returnreal (realvalue: real {value to return} );
{ Leave a real constant "realvalue" on the stack in the place of two
  operands.  Used in constant operation folding.
}
begin {returnreal}
  foldcommon;
  oprndstk[sp].cvalue.representation := reals; {!!!}
  oprndstk[sp].cvalue.realvalue.realbinary := realvalue;
end {returnreal} ;
{>>>}
{<<<}
procedure returnoprnd (i: integer {operand stack index} );
{ Leave operand i on the stack in the place of two operands.
  Used in constant operation folding.
}
var
  o: operand; {temp storage for top of stack}

begin {returnoprnd}
  o := oprndstk[i];
  foldcommon;
  oprndstk[sp] := o;
  oprndstk[sp].value_range := result_range;
end {returnoprnd} ;
{>>>}
{<<<}
procedure returnresult (overflowed: boolean);
{ Return the value computed in result_range and give an error
  if overflowed is true.
}
begin {returnresult}
  with result_range.optimistic do
    returnint(maxlimit, (maxlimit < 0) and not divide_extended);
  if overflowed then warnbefore(overflow);
end {returnresult} ;
{>>>}

{<<<}
procedure dumpconst (constantlen: addressrange; {length of const to be dumped}
                    dumplen: boolean {dump length in stringfile if true} );
{ Dump a constant which is stored as an integer into the string file.
  This is required in two cases: when a structured constant is subjected
  to selection ("[", ".") or when a char or structured constant is
  converted to a string.  If "dumplen" is true the length itself becomes the first byte.
}
  var
    i, j, k: integer; {induction var for putting structured constant}
    tlim: integer; {temp value of consttablelimit}
    kludge: {conversion to string file}
      record
        case boolean of
          true: (b: packed array [1..hostintsize] of hostfilebyte);
          false: (i: integer);
      end;

  begin {dumpconst}
    with oprndstk[sp], cvalue do
      if representation = ints then
        begin
        kludge.i := intvalue;
        if dumplen then
          representation := strings
        else
          begin
          tlim := forcealign (sharedPtr^.stringfilecount, intalign * hostfileunits, false);
          while tlim > sharedPtr^.stringfilecount do
            putbyte (0);
          representation := arrays;
          end;
        pos := sharedPtr^.stringfilecount;

        { This is a structured constant, so the bytes may have to be reversed. }
        i := 1;
        j := hostintsize * hostfileunits;
        if dumplen then
          begin
          putbyte (constantlen);
          oprndlen := oprndlen + 1;
          end;
        if constantlen < hostintsize * hostfileunits then
          if hostintlowbytefirst then j := constantlen {do left part}
          else i := j + 1 - constantlen; {do right part}
        if reversebytes then for k := j downto i do
          putbyte (kludge.b[k])
        else for k := i to j do
          putbyte (kludge.b[k]);
        end;
  end {dumpconst} ;
{>>>}
{<<<}
procedure genbinary (op: operator; {operation to generate}
                    form: types {type of operands} );
  forward;
{ Generate binary operation, see body for details
}
{>>>}

{<<<}
procedure foldneg;
{ Fold a negate operation if the operand is constant }

var
  temp: integer; {used for negation}
  ov: boolean; {set if overflow can occur}

begin
  foldedunary := false;

  with result_range.optimistic do
    begin
    negate(minlimit, temp, ov);
    negate(maxlimit, minlimit, ov);
    maxlimit := temp;
    end;

  with result_range.pessimistic do
    begin
    negate(minlimit, temp, ov);
    negate(maxlimit, minlimit, ov);
    maxlimit := temp;
    end;

  if oconst then
    if (unaryform = reals) or (unaryform = doubles) then
      begin
      oprndstk[sp].cvalue.realvalue.realbuffer := negaterealconst (getrealbuffer(sp));
      foldedunary := true;
      end
    else
      begin
      if oextended then
        if (oprndstk[sp].cvalue.intvalue <> sharedPtr^.targetminint) or
           ((oprndstk[sp].cvalue.intvalue = sharedPtr^.targetminint) and oprndstk[sp].cvalue.negated)
        then
          ov := true
        else
          with result_range do begin  {oops; - (maxint + 1)}
          ov := false;
          pessimistic.extended := false;
          optimistic.extended  := false;
          pessimistic.maxlimit := sharedPtr^.targetminint;
          pessimistic.minlimit := sharedPtr^.targetminint;
          optimistic.maxlimit  := sharedPtr^.targetminint;
          optimistic.minlimit  := sharedPtr^.targetminint;
          oprndstk[sp].cvalue.negated := true;
          oprndstk[sp].value_range := result_range;
          end
      else
        with oprndstk[sp].cvalue do
          begin
          intvalue := result_range.optimistic.maxlimit;
          negated := not negated;
          end;

      if ov then
        warnbefore (overflow);
      foldedunary := true;
      end;
end;
{>>>}
{<<<}
procedure foldrem;
{ Fold the remainder operation.  This operation always comes immediately
  after a divide operation, and extracts the remainder portion of the
  result.  It uses the global "divfolded" which is true if the divide
  could be folded, either to a constant or changed into a shift.
}
  var
    mayoverflow: boolean; {overflow might happen}

  begin {foldrem}
    if freemodwithdiv then
      begin
      mayoverflow := false;
      binaryrange(divide_range, oprndstk[sp].value_range, divide_extended,
                  remainder, usremainder, modrange, result_range, mayoverflow);

      foldedunary := foldedbinary;
      if foldedunary then
        if constcheck(sp - 1) then
          begin
          with result_range.optimistic do
            returnint(maxlimit, (maxlimit < 0) and not divide_extended);
          if mayoverflow then warnbefore(badmodop);
          end
        else
          begin
          with oprndstk[sp].cvalue do
            storagelimit(true, - intvalue, true, intvalue);
          genbinary(andop, unaryform);
          end;
      divfolded := false;
      end;
  end {foldrem} ;
{>>>}
{<<<}
procedure foldquo;
{ Fold a quotient operation.  This operation is always generated immediately
  after a div operation, and extracts the quotient part of the result.  The
  global "divfolded" indicates that the div operation has been folded, either
  to a constant or changed into a shift.
}
  var
    mayoverflow: boolean; {an operation can overflow}

  begin {foldquo}
    if freemodwithdiv then
      begin
      binaryrange(divide_range, oprndstk[sp].value_range, divide_extended,
                  divide, usdivide, divrange, result_range, mayoverflow);

      foldedunary := foldedbinary;
      if foldedunary then
        if constcheck(sp - 1) then
          begin
          with result_range.optimistic do
            returnint(maxlimit, (maxlimit < 0) and not divide_extended);
          if mayoverflow then warnbefore(overflow);
          end
        else if getintvalue(sp) = 0 then returnoprnd(sp - 1)
        else
          begin
          oprndstk[sp].value_range := result_range; {a hack for shiftlop}
          genbinary(shiftlop, unaryform);
          end;
      divfolded := false;
      end;
  end {foldquo} ;
{>>>}
{<<<}
procedure foldmod;
{ Fold the mod operation.  This operation always comes immediately
  after a divide operation, and extracts the remainder portion of the
  result.  It uses the global "foldedbinary" which is true if the divide
  could be folded, either to a constant or changed into a shift.
}
  var
    mayoverflow: boolean; {overflow might happen}
    power2: boolean; { true if divisor ispower of 2}
    power2value: integer; { divisor exponant value }
    typeptr: entryptr; {for access to typeindex data}
    rangenonneg: boolean; {range appears to be nonnegative}

  begin {foldmod}
    if not freemodwithdiv then
      begin
      mayoverflow := false;
      binaryrange(oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                  remainder, usremainder, modrange, result_range, mayoverflow);
      if (lconst and rconst) then returnresult(mayoverflow)
      else if rconst then
        begin
        power2check(getintvalue(r), power2, power2value);
        foldedbinary := false;
        with oprndstk[l] do
          begin
          typeptr := ref(bigtable[typeindex]);
          rangenonneg := value_range.optimistic.minlimit >= 0;
          rangenonneg := rangenonneg or unsigned(typeptr, oprndlen, false);
          end;
        if power2 and rangenonneg then
          begin
          returnoprnd(l);
          pushint( - power2value);
          with oprndstk[sp].cvalue do
            storagelimit(true, - intvalue, true, intvalue);
          binaryop := andop;
          end
        end
      else foldedbinary := false;
      if not foldedbinary then newlen := sharedPtr^.targetintsize;
      end;
  end {foldmod} ;
{>>>}
{<<<}
procedure foldkwo;
{ Fold a quotient operation.  This operation is always generated immediately
  after a div operation, and extracts the quotient part of the result.  The
  global "foldedbinary" indicates that the div operation has been folded,
  either to a constant or changed into a shift.
}
  var
    mayoverflow: boolean; {an operation can overflow}
    power2: boolean; { true if divisor ispower of 2}
    power2value: integer; { divisor exponant value }
    typeptr: entryptr; {for access to typeindex data}
    rangenonneg: boolean; {range appears to be nonnegative}

  begin {foldkwo}
    if not freemodwithdiv then
      begin
      binaryrange (oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                   divide, usdivide, divrange, result_range, mayoverflow);

      if (lconst and rconst) then returnresult(mayoverflow)
      else if rconst and (getintvalue(r) = 1) then returnoprnd(l)
      else if rconst then
        begin
        power2check(getintvalue(r), power2, power2value);
        foldedbinary := false;
        with oprndstk[l] do
          begin
          typeptr := ref(bigtable[typeindex]);
          rangenonneg := value_range.optimistic.minlimit >= 0;
          rangenonneg := rangenonneg or unsigned(typeptr, oprndlen, false);
          end;
        if power2 and rangenonneg then
          begin
          returnoprnd(l);
          { is this right?? 5/14/85}
          oprndstk[sp].value_range := result_range; {a hack for shiftlop}
          pushint( - power2value);
          binaryop := shiftlop;
          end
        else foldedbinary := false;
        end
      else foldedbinary := false;
      if not foldedbinary then newlen := sharedPtr^.targetintsize;
      end;
  end {foldkwo} ;
{>>>}
{<<<}
procedure foldchrarraystr;
{ Turn a character constant or quoted 'Pascal string' into a string,
  if possible.  Note that packed arrays of chars within structured
  constants are NOT folded at this time, as the length byte emitted
  by scan for true string constants is not present in such structures.
  This is a kludge, but strings are a kludge in Pascal (standard and
  extended both) and I didn't invent them!
}

  begin {foldchrarraystr}
    newstringtype(resulttype, strings, olen);
    newresulttype(resulttype);
    olen := olen + 1;
    if constcheck(sp) then
      begin
      dumpconst(olen - 1, true);
      with oprndstk[sp], cvalue do
        if representation = arrays then
          if stringconstflag then
            begin
            pos := pos - 1;
            oprndlen := oprndlen + 1;
            end
          else foldedunary := false;
      end
    else foldedunary := false;
  end {foldchrarraystr} ;
{>>>}
{<<<}
procedure foldfloat;
{ Float an integer constant if this is possible.  This sets the result
  type and size to real no matter what.
}
  var
    tival: integer; {temp storage for integer value}

  begin {foldfloat}
    newresulttype(realindex);
    olen := sharedPtr^.targetrealsize;
    oprndstk[sp].oprndlen := sharedPtr^.targetrealsize;
    if not realfolding then foldedunary := false
    else if oconst then
      begin
      tival := getintvalue(sp);
      oprndstk[sp].cvalue.representation := reals;
      oprndstk[sp].cvalue.realvalue.realbinary := tival;
      end
    else foldedunary := false;
  end {foldfloat} ;
{>>>}
{<<<}
procedure fold_double_float;
{ Float an integer constant if this is possible.  This sets the result
  type and size to double no matter what.
}

  var
    tival: integer; {temp storage for integer value}

  begin {fold_double_float}
    newresulttype(doubleindex);
    olen := doublesize;
    oprndstk[sp].oprndlen := doublesize;
    if not realfolding then foldedunary := false
    else if oconst then
      begin
      tival := getintvalue(sp);
      oprndstk[sp].cvalue.representation := doubles;
      oprndstk[sp].cvalue.realvalue.realbinary := tival;
      end
    else foldedunary := false;
  end {fold_double_float} ;
{>>>}
{<<<}
procedure foldpush;
{ Change a push to a pushlit if possible.  This applies to literal
  zero of any flavor, including nil pointers, or to integer literals.
}
  var
    i: integer; {value if foldable}

  begin {foldpush}
    if unaryform = ints then olen := max(olen, stackalign);
    foldedunary := oconst;
    if oconst then
      case unaryform of
        reals:
          begin
          if realfolding then foldedunary := getrealvalue(sp) = 0.0
          else foldedunary := false;
          i := 0;
          end;
        ints, scalars, bools, chars: i := getintvalue(sp);
        ptrs: i := niladdressvalue;
        otherwise foldedunary := false;
        end;
    if foldedunary then
      begin
      genlit(i);
      genop (pushlitvalue);
      genint (olen);
      genint (ocost);
      genform (unaryform);
      oprndstk[sp].operandkind := exproperand;
      oprndstk[sp].cost := 0;
      end;
  end {foldpush} ;
{>>>}
{<<<}
procedure foldnot;
{ Fold a boolean or integer "not" operation if possible }

var
  temp: integer; {used in "not-ing" the range}

begin
  foldedunary := oconst;
  with oprndstk[sp], cvalue do
    if oconst then
      begin
      if unaryform = bools then
        intvalue := 1 - intvalue
      else
        intvalue := not intvalue;
      setconstrange (intvalue, negated, result_range);
      end
    else if unaryform <> bools then
      begin
      with result_range.optimistic do
        begin
        temp := not minlimit;
        minlimit := not maxlimit;
        maxlimit := temp;
        end;
      with result_range.pessimistic do
        begin
        temp := not minlimit;
        minlimit := not maxlimit;
        maxlimit := temp;
        end;
      end;
end;
{>>>}
{<<<}
procedure foldsetelt;
{ Fold a single set member-designator by inserting the bit into a constant set if the operand is constant }

begin
  foldedunary := oconst;
  if foldedunary then
    begin
    with oprndstk[sp - 1].cvalue do
      if (representation = sets) and (getintvalue(sp) >= 0) and
         (getintvalue(sp) <= maxsetord) then
        setvalue^ := setvalue^ + [getintvalue(sp)]
      else
        warnbefore (bigsetbase);

    oprndstk[sp].operandkind := exproperand;
    oprndstk[sp].cost := 0;
    end;
end;
{>>>}
{<<<}
procedure foldchk (adjustlow: boolean; {adjust lower bound to 0}
                  chkerror: warning; {message to issue if error}
                  generate: boolean {generate the check} );
{ Do the preliminary work for a range check.  If the operand being checked
  is constant, the check is done at compile time, with the error
  message provided by "chkerror".  If "adjustlow" is true, the
  constant value will be changed by the value of the low bound to give
  a range of 0..(high-low).  This is used for index range checks
  only.  If the operand is not constant, literal high and low values
  are written to the intermediate file for use by the rangechkop
  which will be generated by the main "genunary" routine.
}

  var
    outofbounds: boolean; {can't possibly be in range}
    upperneeded: boolean; {upper limit check needed}
    lowerneeded: boolean; {lower limit check needed}
    typeptr: entryptr; {for access to type}
    lowerbound: integer; {lower bound of typeindex}
    upperbound: integer; {upper bound of typeindex}

  begin {foldchk}
    typeptr := ref(bigtable[oprndstk[sp].typeindex]);
    lowerbound := lower(typeptr);
    upperbound := upper(typeptr);
    if typeptr^.typ in [subranges, chars, bools, ints, scalars] then
      begin
      checkrange(oprndstk[sp], false, outofbounds, lowerneeded, upperneeded);
      if oconst then
        begin
        if outofbounds then warnbefore(chkerror);
        if adjustlow then
          oprndstk[sp].cvalue.intvalue := oprndstk[sp].cvalue.intvalue -
                                          lowerbound;
        end
      else if generate and (upperneeded or lowerneeded) or adjustlow and
              (lowerbound <> 0) then
        begin
        foldedunary := false;
        pushint(lowerbound);
        genoprnd;
        pushint(upperbound);
        genoprnd;
        if adjustlow then
          with result_range do
            begin
            optimistic.minlimit := optimistic.minlimit - lowerbound;
            optimistic.maxlimit := optimistic.maxlimit - lowerbound;
            pessimistic := optimistic;
            end;
        end;
      end
    else foldedunary := generate;
  end {foldchk} ;
{>>>}
{<<<}
procedure foldunary;
{ Check the operands of those unary operations which can possibly be
  folded and fold the operation if possible.  Note: in the case of
  range checks, this may result in some intermediate file code being
  generated for the range limit values if the operand is not a constant.
}

  begin {foldunary}
    foldedunary := true;
    case unaryop of
      pushvalue: foldpush;
      setelt: foldsetelt;
      notop: foldnot;
      negop: foldneg;
      remop: foldrem;
      quoop: foldquo;
      float: foldfloat;
      float_double: fold_double_float;
      chrstrop, arraystrop: foldchrarraystr;
      indxchkop: foldchk(true, indexerror, sharedPtr^.switchcounters[indexcheck] > 0);
      congruchkop: foldchk(false, indexerror, sharedPtr^.switchcounters[indexcheck] > 0);
      rangechkop: foldchk(false, rangeerror, sharedPtr^.switchcounters[rangecheck] > 0);
      otherwise foldedunary := false
      end;
  end {foldunary} ;
{>>>}
{<<<}
procedure genunary (op: operator; {operation to generate}
                   form: types {type of operand} );
{ Generate a unary operation.  The operands are first checked to
  see if they can be folded.  If not, the operation is generated with
  the form:
        op(length, cost, form)
  The operand stack is adjusted to reflect the result of the operation.
}

  begin {genunary}

    if sp >= 0 then
      begin

      unaryop := op;
      unaryform := form; {ugh}
      result_range := oprndstk[sp].value_range;

      oconst := constcheck(sp);
      ocost := max(1, computecost(sp));
      olen := oprndstk[sp].oprndlen;
      oextended := oprndstk[sp].extended;

      foldunary;

      if not (op in
         [bldfmt, filebufindrop, float, indrop, indxop, ptrchkop, pushaddr,
         pushvalue, pushfinal, pushproc, paindxop, pindxop, call, callparam,
         unscall, unscallparam, chrstrop, arraystrop]) and (form = ints) then
        begin
        olen := range_length(result_range.optimistic);
        if (not foldedunary and (oprndstk[sp].oprndlen > olen)) or
           (sharedPtr^.switchcounters[truncatesw] > 0) then
          olen := oprndstk[sp].oprndlen;
        end;

      if not foldedunary then
        begin
        genoprnd;
        sp := sp + 1;
        genop(op);
        genint(olen);
        genint(ocost);
        genform(form);
        end;

      with oprndstk[sp] do
        begin
        typeindex := resulttype;
        value_range := result_range;
        if not foldedunary then
          begin
          operandkind := exproperand;
          cost := ocost;
          oprndlen := olen;
          end;
        end;
      end;
    resultptr := ref(bigtable[resulttype]);
  end {genunary} ;
{>>>}
{<<<}
procedure foldintplusminus (sign: integer {1 if add, -1 if sub} );
{ Fold integer addition or subtraction.  This can be done if both
  operands are constant, or if either has constant value zero.  If
  overflow would result, an error message is generated.  Also,
  if one of the operands has constant value one, the operation is
  converted into an increment or decrement operation.
  **** NOTE ****  if the global "linearize" is set, a constant
  right operand will be placed into the linear factor for use in
  array accessing.
}
  var
    mayoverflow: boolean; {overflow is possible this op}

  begin {foldintplusminus}
    if sign > 0 then
      binaryrange (oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                   addProc, usadd, addrange, result_range, mayoverflow)
    else
      binaryrange (oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                   subtract, ussubtract, subrange, result_range, mayoverflow);

    if (lconst and rconst) then returnresult(mayoverflow)
    else if rconst and linearize then
      begin
      linearfactor := linearfactor + sign * getintvalue(r);
      returnoprnd (l);
      end
    else if lconst and (getintvalue(l) = 0) and (sign > 0) then
      returnoprnd (r)
    else if rconst and (getintvalue(r) = 0) then
      returnoprnd (l)
    else if rconst and (getintvalue(r) = 1) then
      begin
      returnoprnd (l);
      if sign = 1 then
        genunary(incop, binaryform)
      else
        genunary (decop, binaryform);
      end
    else if lconst and (getintvalue(l) = 1) and (sign > 0) then
      begin
      returnoprnd (r);
      genunary (incop, binaryform)
      end
    else
      begin
      newlen := sharedPtr^.targetintsize;
      foldedbinary := false;
      end;
  end;
{>>>}
{<<<}
procedure foldrealplusminus (sign: integer {1 if add, -1 if sub} );
{ Fold addition or subtraction for real operands.  This is possible if
  both operands are constant, or if either operand is constant zero.
  The case where the left operand is constant zero and the operation is
  subtraction could be folded to a negate operation, but this is not
  done at the present.
}

  begin {foldrealplusminus}
    if not realfolding then
      foldedbinary := false
    else if (lconst and rconst) then
      returnreal(getrealvalue(l) + getrealvalue(r) * sign)
    else if lconst and (getrealvalue(l) = 0.0) and (sign > 0) then
      returnoprnd(r)
    else if rconst and (getrealvalue(r) = 0.0) then
      returnoprnd(l)
    else
      foldedbinary := false;
  end {foldrealplusminus} ;
{>>>}
{<<<}
procedure foldstringplus;
{ Fold string concatenation operator.
  Actually, we don't fold at all at this point but we do create the
  new type created by the operation!
}

  begin {foldstringplus}
    foldedbinary := false;
    newlen := min(maxstrlen + 1,
                  oprndstk[l].oprndlen + oprndstk[r].oprndlen - 1);
    newstringtype(resulttype, strings, newlen - 1); {newstringtype adds 1}
  end {foldstringplus} ;
{>>>}
{<<<}
procedure foldplusminus (sign: integer {1 if add, -1 if sub} );
{ Fold addition and subtraction.
}
  begin {foldplusminus}
    if binaryform = strings then foldstringplus
    else if (binaryform = ints) or (binaryform = subranges) then
      foldintplusminus(sign)
    else if binaryform = reals then
      foldrealplusminus(sign) {!!!}
    else
      foldedbinary := false;
  end {foldplusminus} ;
{>>>}
{<<<}
procedure foldintmul;
{ Fold an integer multiply if possible.  This can be done if both operands
  are constant, or if either is constant one.  Also, if an operand is
  a constant power of two the operation can be converted into a shift.
  Overflow is possible, and will result in a compile-time message.
  There are several optimizations which are possible but not yet
  incorporated into the routine.  For instance, negative constant
  multipliers could be handled.  Also, the case of the left being
  constant is not handled.
}
  var
    power2: boolean; {set if operand is power of 2}
    power2value: integer; {resulting shift distance}
    mayoverflow: boolean; {operation might overflow}

  begin {foldintmul}
    binaryrange (oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                 multiply, usmultiply, mulrange, result_range, mayoverflow);

    if (lconst and rconst) then
      returnresult(mayoverflow)
    else if lconst and (getintvalue(l) = 1) then
      returnoprnd(r)
    else if rconst and (getintvalue(r) = 1) then
      returnoprnd(l)
    else if rconst then
      begin
      power2check (getintvalue(r), power2, power2value);
      if power2 then
        begin
        returnoprnd (l);
        pushint (power2value);
        binaryop := shiftlop;
        foldedbinary := false;
        end
      else
        foldedbinary := false;
      end
    else
      foldedbinary := false;
    if not foldedbinary then
      newlen := sharedPtr^.targetintsize;
  end;
{>>>}
{<<<}
procedure foldrealmul;
{ Fold a real multiply.  This is possible if both operands are constant,
  or if either is constant one.
}

  begin {foldrealmul}
    if not realfolding then foldedbinary := false
    else if (lconst and rconst) then
      returnreal(getrealvalue(l) * getrealvalue(r))
    else if lconst and (getrealvalue(l) = 1.0) then returnoprnd(r)
    else if rconst and (getrealvalue(r) = 1.0) then returnoprnd(l)
    else foldedbinary := false;
  end {foldrealmul} ;
{>>>}
{<<<}
procedure foldmul;
{ Fold a multiply operation if possible }

  begin {foldmul}
    if (binaryform = ints) or (binaryform = subranges) then foldintmul
    else if binaryform = reals then foldrealmul
    else foldedbinary := false;
  end {foldmul} ;
{>>>}
{<<<}
procedure folddiv;
{ Check the operands of a div operand and see if they can be folded.
  Since this operation is always followed by a remainder or quotient
  operation to extract the needed part of the answer, it does not
  actually do the folding.  It sets the global flag "divfolded" which
  informs the "genunary" routine that folding is possible.  If the
  divisor is an even power of two, and the left operand is a positive
  subrange (i.e. 1..10), that power is substituted in
  that operand, and will be converted to a shift or and operation.
}
  var
    power2: boolean; {true if divisor is a power of 2}
    power2value: integer; {divisor exponent value}
    typeptr: entryptr; {for access to typeindex data}
    rangenonneg: boolean; {range appears to be nonnegative}

  begin {folddiv}
    if freemodwithdiv then
      begin
      if rconst and not lconst then
        begin
        power2check(getintvalue(r), power2, power2value);
        with oprndstk[l] do
          begin
          typeptr := ref(bigtable[typeindex]);
          rangenonneg := value_range.optimistic.minlimit >= 0;
          if power2 and (rangenonneg or unsigned(typeptr, oprndlen, false)) then
            oprndstk[r].cvalue.intvalue := - power2value
          else foldedbinary := false;
          divfolded := foldedbinary;
          end;
        end
      else foldedbinary := lconst and rconst;
      if not foldedbinary then newlen := sharedPtr^.targetintsize;
      divide_range := oprndstk[l].value_range;
      divide_extended := oextended;
      end;
  end {folddiv} ;
{>>>}
{<<<}
procedure foldslash;
{ Fold a real divide.  This will be possible if both are constant, or if
  the left is constant zero, or the right is constant one.
  If this would cause an error, it is left until runtime for it to occur.
}

  begin {foldslash}
    if not realfolding then foldedbinary := false
    else if rconst and (getrealvalue(r) = 0.0) then warnbefore(overflow)
    else if (lconst and rconst) then
      returnreal(getrealvalue(l) / getrealvalue(r))
    else if rconst and (getrealvalue(r) = 1.0) then returnoprnd(l)
    else foldedbinary := false;
  end {foldslash} ;
{>>>}
{<<<}
procedure foldcmp (op: operator {operation being folded} );
{ Fold moves and compares.  If both operands are constant (obviously
  not a move), the comparison is folded into a boolean constant so
  that travrs can "deaden" any code that depends on NOT the result.
  If only one operand is constant, the move or compare operation is
  folded to a movelit or cmplit of some sort.
}

  var
    i: integer; {constant values for compare}
    uns: boolean; {use unsigned comparisons on integers}
    temp: operand; {used for swapping operands}
    result: boolean; { result of folded compare }

  begin {foldcmp}
    foldedbinary := false;
    if rconst and lconst and
       (binaryform in
       [bools, chars, ints, ptrs, reals, doubles, scalars, subranges]) then
      begin
      case binaryform of
        bools, chars, ints, scalars, subranges:
          begin
          uns := oprndstk[l].value_range.optimistic.extended or
                 oprndstk[r].value_range.optimistic.extended;
          if uns then uscompareint(getintvalue(l), getintvalue(r), result, op)
          else compareint(getintvalue(l), getintvalue(r), result, op);
          foldedbinary := true;
          end;
        ptrs:
          begin
          result := op = eqlit; {both NIL}
          foldedbinary := true;
          end;
        reals, doubles:
          if realfolding then
            begin
            comparereal(getrealvalue(l), getrealvalue(r), result, op);
            foldedbinary := true;
            end;
        otherwise {foldedbinary := false} ;
        end {case binaryform} ;
      if foldedbinary then
        begin
        returnint(ord(result), false);
        setconstrange(ord(result), false, result_range);
        newresulttype(boolindex);
        binaryform := bools;
        oprndstk[sp].oprndlen := unitsize;
        end;
      end;

    { Correct for mismatched length in case of the empty set }
    if binaryform = sets then
      newlen := min(oprndstk[l].oprndlen, oprndstk[r].oprndlen);

    if (rconst <> lconst) then
      begin
      foldedbinary := true;
      if ((binaryform = strings) or (binaryform = arrays)) then
        newlen := resultptr^.arraymembers
        {???}
      else if binaryform = ints then
        if range_length(oprndstk[l].value_range.optimistic) >
           range_length(result_range.optimistic) then
          result_range := oprndstk[l].value_range;
      {???}
      if not rconst and
         (binaryform in
         [bools, chars, ints, ptrs, reals, doubles, scalars, subranges]) then
        begin {reverse the operands}
        temp := oprndstk[l];
        oprndstk[l] := oprndstk[r];
        oprndstk[r] := temp;
        lconst := false;
        rconst := true;
        {and reverse any comparisons}
        case binaryop of
          lssop:
            begin
            op := gtrlit;
            binaryop := gtrop;
            end;
          leqop:
            begin
            op := geqlit;
            binaryop := geqop;
            end;
          gtrop:
            begin
            op := lsslit;
            binaryop := lssop;
            end;
          geqop:
            begin
            op := leqlit;
            binaryop := leqop;
            end;
          otherwise {no change} ;
          end {case} ;
        end;
      case binaryform of
        bools, chars, ints, scalars, subranges: i := getintvalue(r);
        ptrs: i := niladdressvalue;
        reals, doubles:
          begin
          if realfolding then foldedbinary := getrealvalue(r) = 0.0
          else foldedbinary := false;
          i := 0;
          end;
        otherwise foldedbinary := false;
        end;
      if foldedbinary then
        begin
        genlit(i);
        returnoprnd(l);
        genunary(op, binaryform);
        end;
      end;
  end {foldcmp} ;
{>>>}
{<<<}
procedure foldmove;
{ Fold moves, folding to a movelit if the right operand is constant }

  var
    i: integer; {constant values for move}
    f: entryptr; {to get at a form entry}

  begin {foldmove}
    foldedbinary := false;

    { Correct for mismatched length in case of the empty set }
    if binaryform = sets then
      newlen := min(oprndstk[l].oprndlen, oprndstk[r].oprndlen);

    if rconst then
      begin
      foldedbinary := true;
      {???}
      if binaryform = ints then
        if range_length(oprndstk[l].value_range.optimistic) >
           range_length(result_range.optimistic) then
          result_range := oprndstk[l].value_range;
      {???}
      case binaryform of
        bools, chars, ints, scalars, subranges: i := getintvalue(r);
        ptrs: i := niladdressvalue;
        reals, doubles:
          begin
          if realfolding then foldedbinary := getrealvalue(r) = 0.0
          else foldedbinary := false;
          i := 0;
          end;
        otherwise foldedbinary := false;
        end;
      if foldedbinary then
        begin
        genlit(i);
        returnoprnd(l);
        genunary(movelit, binaryform);
        end;
      end;
  end {foldmove} ;
{>>>}
{<<<}
procedure foldsetpair;
{ Fold a set constructor of the form a..b into a constant set.  This is possible only if both are constant }

  var
    i: integer; {induction var}

  begin {foldsetpair}
    foldedbinary := lconst and rconst;
    if foldedbinary then
      begin
      with oprndstk[sp - 2].cvalue do
        if (getintvalue(sp - 1) >= 0) and (getintvalue(sp - 1) <= maxsetord) and
           (getintvalue(sp) >= 0) and (getintvalue(sp) <= maxsetord) then
          setvalue^ := setvalue^ + [getintvalue(sp - 1)..getintvalue(sp)]
        else warnbefore(bigsetbase);
      sp := sp - 1;
      oprndstk[sp].operandkind := exproperand;
      oprndstk[sp].cost := 0;
      end;
  end {foldsetpair} ;
{>>>}
{<<<}
procedure foldand;
{ Fold integer and boolean 'and' operator }

  var
    i: integer; {constant value}

  begin {foldand}
    andrange(oprndstk[l].value_range.optimistic,
             oprndstk[r].value_range.optimistic, result_range.optimistic);
    andrange(oprndstk[l].value_range.pessimistic,
             oprndstk[r].value_range.pessimistic, result_range.pessimistic);

    if lconst and rconst then returnresult(false)
    else if lconst then
      begin
      i := getintvalue(l);
      if (binaryform = bools) and (i = 1) or (binaryform = ints) and
         (i = - 1) then
        returnoprnd(r)
      else foldedbinary := false;
      end
    else if rconst then
      begin
      i := getintvalue(r);
      if (binaryform = bools) and (i = 1) or (binaryform = ints) and
         (i = - 1) then
        returnoprnd(l)
      else foldedbinary := false;
      end
    else foldedbinary := false;
  end {foldand} ;
{>>>}
{<<<}
procedure foldor;
{ Fold integer and boolean 'or' operator }

  begin {foldor}
    orrange(oprndstk[l].value_range.optimistic,
            oprndstk[r].value_range.optimistic, result_range.optimistic);
    orrange(oprndstk[l].value_range.pessimistic,
            oprndstk[r].value_range.pessimistic, result_range.pessimistic);
    if lconst and rconst then returnresult(false)
    else if lconst and (binaryform in [bools, ints]) and
            (getintvalue(l) = 0) then
      returnoprnd(r)
    else if rconst and (binaryform in [bools, ints]) and
            (getintvalue(r) = 0) then
      returnoprnd(l)
    else foldedbinary := false;
  end {foldor} ;
{>>>}
{<<<}
procedure foldbinary;
{ Fold a binary operation if this is possible.  Only constant operands
  may be discarded, since intermediate code for expressions is generated
  on the fly.  This limits folding possibilities slightly, since expressions
  like "a and false" (with "a" boolean) can not be discarded }

  begin {foldbinary}
    foldedbinary := true;
    case binaryop of
      plusop: foldplusminus(1);
      minusop: foldplusminus( - 1);
      mulop: foldmul;
      divop, stddivop: folddiv;
      stdmodop, modop: foldmod;
      kwoop: foldkwo;
      slashop: foldslash;
      moveop: foldmove;
      lssop: foldcmp(lsslit);
      leqop: foldcmp(leqlit);
      gtrop: foldcmp(gtrlit);
      geqop: foldcmp(geqlit);
      eqop: foldcmp(eqlit);
      neqop: foldcmp(neqlit);
      setpair: foldsetpair;
      andop: foldand;
      orop: foldor;
      otherwise foldedbinary := false
      end
  end {foldbinary} ;
{>>>}

{<<<}
procedure genbinary { op:operator; (operation to generate) form: types (type of operands)} ;
{ Generate intermediate file output for a binary operation.  If possible,
  the operation will be folded.  The operand stack is updated to reflect
  the result of the operation.
}
var
  lrange, rrange: addressrange; {size of operands, based on range}

begin
  if sp >= 1 then
    begin
    l := sp - 1;
    r := sp;
    lconst := constcheck(l);
    rconst := constcheck(r);
    c1 := computecost(l);
    c2 := computecost(r);
    result_range := oprndstk[r].value_range;
    newlen := max(oprndstk[l].oprndlen, oprndstk[r].oprndlen);
    if c1 = c2 then newcost := c1 + 1
    else newcost := max(c1, c2);
    oextended := oprndstk[l].extended or oprndstk[r].extended;
    binaryop := op; {may be modified by foldbinary}
    binaryform := form; {may be modified by foldbinary}

    foldbinary;

    if (binaryform = ints) and
       not (binaryop in [indxop, aindxop, addrop, indrop] {addressing operators} ) then
      begin
      lrange := range_length(oprndstk[l].value_range.optimistic);
      rrange := range_length(oprndstk[r].value_range.optimistic);
      if sharedPtr^.switchcounters[truncatesw] <= 0 then
        begin
        newlen := range_length(result_range.optimistic);
        if divfolded or not foldedbinary then
          begin
          if lrange > newlen then
            newlen := lrange;
          if rrange > newlen then
            newlen := rrange;
          end;
        end
      else {truncate}
        newlen := max (lrange, rrange);
      end;
    if not foldedbinary then
      begin
      genoprnd;
      genoprnd;
      if lconst then
        genop (switchstack);
      sp := sp + 1;
      genop (binaryop);
      genint (newlen);
      genint (newcost);
      genform (binaryform);
      end;

    with oprndstk[sp] do
      begin
      typeindex := resulttype;
      value_range := result_range;
      if not foldedbinary then
        begin
        oprndlen := newlen;
        operandkind := exproperand;
        cost := newcost;
        end;
      end;
    end;
  resultptr := ref(bigtable[resulttype]);
end;
{>>>}
{<<<}
procedure setdefaulttargetintsize;
{ Used to force operand to be of length "defaulttargetintsize".  In
  particular, integer arguments to many support library routines must
  be extended to this length.
}
begin {setdefaulttargetintsize}
  oprndstk[sp].oprndlen := defaulttargetintsize;
end {setdefaulttargetintsize} ;
{>>>}
{<<<}
procedure genpushint (i: integer);
{ Cause i to be pushed at runtime. }

begin {genpushint}
  pushint(i);
  genunary(pushvalue, ints);
end {genpushint} ;
{>>>}
{<<<}
procedure genpushdefaultint (i: integer);
{ Cause i to be pushed at runtime. }

begin {genpushdefaultint}
  pushint(i);
  setdefaulttargetintsize;
  genunary(pushvalue, ints);
end {genpushdefaultint} ;
{>>>}
{<<<}
procedure genpushbool (b: boolean);
{ Cause b to be pushed at runtime. }

begin {genpushbool}
  pushint(ord(b));
  oprndstk[sp].typeindex := boolindex;
  oprndstk[sp].oprndlen := 1;
  genunary(pushvalue, bools);
end {genpushbool} ;
{>>>}
{<<<}
procedure computeresult (maybestring: boolean {force char to string?});
{ Check the two operands of a binary operation for type compatibility.
  If one side is real or double and the other integer, the integer operand is
  converted to real or double before the check.  Real is also promoted to
  double when mixed.

  The conversion is complicated by the fact that the code to access the
  left operand was generated before the decision could be made.  This
  is handled by issuing a "switchstack" operator in the intermediate
  file, doing the float, and issuing another "switchstack" to restore
  the state.

  The maybestring flag is a kludge to help in cases like 'a' + 'b', i.e.
  two chars which the programmer imagines to be strings.  It is true when
  ever the operands MIGHT be allowable as strings.
}

type
  castreal = record
    case boolean of
      true:  (b: realarray);
      false: (s: real)
  end;

var
  lefttype, righttype: tableIndex; {formentry's for operands}
  f: entryptr; {used for access to formentries}
  leftshortstring, rightshortstring: boolean; {left or right is/are short
                                               'standard' strings}
  leftstringtype: boolean; {left type is packed array [1..n] of char}
  leftform, rightform: types; {operand types}
  tival: integer; {temporary integer value}
  bscast: castreal;

begin {computeresult}
  if sp > 0 then lefttype := oprndstk[sp - 1].typeindex
  else lefttype := noneindex;
  if sp >= 0 then righttype := oprndstk[sp].typeindex
  else righttype := noneindex;
  newresulttype(noneindex);

  f := ref(bigtable[lefttype]);
  leftform := getform(f);
  leftstringtype := (leftform = arrays) and f^.stringtype;
  leftshortstring := leftstringtype and (f^.arraymembers = 1);

  f := ref(bigtable[righttype]);
  rightform := getform(f);
  rightshortstring := (rightform = arrays) and f^.stringtype and (f^.arraymembers = 1);
  if leftshortstring and (rightform = chars) and
     (sharedPtr^.switchcounters[standard] <= 0) then
    begin
    newstringtype(resulttype, arrays, 1);
    newresulttype(resulttype);
    oprndstk[sp].typeindex := resulttype;
    end
  else if rightshortstring and (leftform = chars) and
          (sharedPtr^.switchcounters[standard] <= 0) then
    begin
    newstringtype(resulttype, arrays, 1);
    newresulttype(resulttype);
    oprndstk[sp - 1].typeindex := resulttype;
    end
  else
    begin
    if (maybestring and ((leftform = chars) or leftstringtype) or
       (leftform = strings)) and (rightform = chars) then
      begin
      genunary(chrstrop, strings);
      righttype := resulttype;
      rightform := strings;
      end;
    if (maybestring and ((leftform = chars) or leftstringtype) or
       (leftform = strings)) and (rightform = arrays) and
       f^.stringtype then
      begin
      genunary(arraystrop, strings);
      righttype := resulttype;
      rightform := strings;
      end;
    if (rightform = strings) and (leftstringtype or
       (leftform = chars)) then
      begin
      sp := sp - 1;
      if not constcheck(sp) and not constcheck(sp + 1) then
        genop(switchstack);
      if leftstringtype then genunary(arraystrop, strings)
      else genunary(chrstrop, strings);
      if not constcheck(sp) and not constcheck(sp + 1) then
        genop(switchstack);
      sp := sp + 1;
      end
    else if (leftform = ints) and (rightform = reals) then
      begin
      if not constcheck(sp - 1) or not realfolding then
        begin
        if not constcheck(sp) and not constcheck(sp - 1) then
          genop(switchstack);
        sp := sp - 1;
        genunary(float, ints);
        sp := sp + 1;
        if not constcheck(sp) then genop(switchstack);
        end
      else if realfolding then
        with oprndstk[sp - 1], cvalue do
          begin
          tival := intvalue;
          typeindex := realindex;
          representation := reals;
          realvalue.realbinary := tival;
          end;
      newresulttype(realindex);
      end
    else if (leftform = reals) and (rightform = ints) then
      begin
      genunary(float, ints);
      newresulttype(realindex);
      end
    else if (leftform = ints) and (rightform = doubles) then
      begin
      if not constcheck(sp - 1) or not realfolding then
        begin
        if not constcheck(sp) and not constcheck(sp - 1) then
          genop(switchstack);
        sp := sp - 1;
        genunary(float_double, ints);
        sp := sp + 1;
        if not constcheck(sp) then genop(switchstack);
        end
      else if realfolding then
        with oprndstk[sp - 1], cvalue do
          begin
          tival := intvalue;
          typeindex := doubleindex;
          representation := doubles;
          realvalue.realbinary := tival;
          end;
      newresulttype(doubleindex);
      end
    else if (leftform = doubles) and (rightform = ints) then
      begin
      genunary(float_double, ints);
      newresulttype(doubleindex);
      end
    else if (leftform = reals) and (rightform = doubles) then
      begin
      if not constcheck(sp - 1) or not realfolding then
        begin
        if not constcheck(sp) and not constcheck(sp - 1) then
          genop(switchstack);
        sp := sp - 1;
        oprndstk[sp].oprndlen := doublesize;
        genunary(real_to_dbl, reals);
        sp := sp + 1;
        if not constcheck(sp) then genop(switchstack);
        end
      else
        with oprndstk[sp - 1], cvalue do
          begin
          bscast.b := realvalue.realbuffer;
          realvalue.realbinary := bscast.s;
          typeindex := doubleindex;
          representation := doubles;
          end;
      newresulttype(doubleindex);
      end
    else if (leftform = doubles) and (rightform = reals) then
      begin
      oprndstk[sp].oprndlen := doublesize;
      genunary(real_to_dbl, reals);
      newresulttype(doubleindex);
      end
    else if compatible(lefttype, righttype) then newresulttype(lefttype)
    else warnbefore(typesincomp)
    end;
end {computeresult} ;
{>>>}
{<<<}
procedure gencheck (op: operator; {operator to generate}
                   checktype: tableIndex {type for check} );
{ If the top of the operand stack can be checked, check it. }

var
  generate: boolean; {actually generate a check}
  checkptr: entryptr; {for access to checktype entry}

begin {gencheck}
  checkptr := ref(bigtable[checktype]);
  with checkptr^ do
    if typ = subranges then
      generate := (lowerord <> 0) or (upperord <> maxusint)
    else generate := typ in [bools, chars, scalars];
  if generate then
    begin
    oprndstk[sp].typeindex := checktype;
    genunary(op, ints);
    end;
end {gencheck} ;
{>>>}

{<<<}
procedure setshorttargetintsize;
{ Used to force operand to be of length "shorttargetintsize".  Certain
  support library routines (string intrinsics, at least) don't need full
  integers.  Checking code is emitted to ensure that nothing naughty goes
  on behind our backs.
}
begin {setshorttargetintsize}
  gencheck(rangechkop, shortintindex);
  oprndstk[sp].oprndlen := shorttargetintsize;
end {setshorttargetintsize} ;
{>>>}
{<<<}
function checkforstack (varindex: tableIndex; {variable to check}
                        var where: forstackindex {for level} ): boolean;
{ Check a variable to see if it is being used as the controlled variable
  in a for statement.  As for statements are encountered, the index of the
  controlled variable is stored in a stack in the compiler.  This allows the
  compiler to check for the assignment rules, and to allocate this variable
  to a register for the duration of the for statement.
}
var
  i: forstackindex; {induction var for search}

begin {checkforstack}
  forstack[0].forindex := varindex;
  i := forsp;
  while forstack[i].forindex <> varindex do i := i - 1;
  where := i;
  checkforstack := (i <> 0);
end {checkforstack} ;
{>>>}

{<<<}
procedure getlevel (lev: levelindex; {level to reference}
                   param: boolean {true if parameter} );
{ Generate the intermediate file code for a reference to the level
  requested.  The operand stack is set up with an address operand.
  Code generated:
        get-level = "globalop" | "originop" |
                ( "localop"
                  [* "levop(level, [0 | blockref])" *] )  .

  Note: origin code depends on level 0 being equal to a dummy
  absolute 0 address reference
}
var
  i: levelindex; {induction var for levop generation}

begin {getlevel}
  if lev = 0 then
    begin
    genop(levop);
    genint(0);
    genint(0);
    end
  else if lev = 1 then genop(globalop)
  else
    begin
    if not param or (lev < level) then genop(localop);
    for i := level - 1 downto lev + 1 do
      begin
      genop(levop);
      genint(i);
      genint(0);
      end;
    if param or (lev < level) then
      begin
      genop(levop);
      genint(lev);
      if param then
        genint (sharedPtr^.blockref)
      else
        genint(0);
      end;
    end;
  bumpsp;
  with oprndstk[sp] do
    begin
    typeindex := intindex;
    oprndlen := sharedPtr^.ptrsize;
    extended := false;
    operandkind := exproperand;
    cost := 0
    end;
end {getlevel} ;
{>>>}

{<<<}
procedure setnowunpacking (packedflag: boolean; {packed structure?}
                          offset: addressrange; {start of field}
                          var nowunpacking: boolean {result} );
{ Check a field being accessed and set "nowunpacking" if we need to
  generate unpacking code.  It is possible for the structure to be packed,
  but the particular field falls on a word boundry, and no unpacking code
  is necessary.
  This routine uses the global "resultptr" to determine size.
}
begin {setnowunpacking}
  nowunpacking := packedflag and ((offset mod bitsperunit <> 0) or
                  ((offset + sizeof(resultptr, true)) mod bitsperunit <> 0));
end {setnowunpacking} ;
{>>>}
{<<<}
procedure startunpacking (nowunpacking: boolean; {unpacking now?}
                         var constpart: addressrange; {const address}
                         var unpacking: boolean {been unpacking?} );
{ If we are just beginning to unpack a field, generate the constant part
  of the word address and set the "unpacking" flag.  The constpart will
  now be a bit address, so is set to zero.
}
begin {startunpacking}
  if nowunpacking and not unpacking then
    begin
    if constpart <> 0 then
      begin
      genlit(constpart);
      oprndstk[sp].oprndlen := unitsize;
      genunary(indxop, ints);
      end;
    constpart := 0;
    unpacking := true;
    end;
end {startunpacking} ;
{>>>}
{<<<}
procedure lastindex (var unpacking: boolean; {unpacking a field}
                    constpart: addressrange; {addr const part}
                    var len: addressrange {operand length} );
{ Generates the last "indxop" in accessing a variable.
  Output generated:
        "lit(constpart)"
        ( pindxop(len, cost, 'ints')" |
          indxop(len, cost, 'ints')" )  .
}
var
  power2: boolean;
  power2dummy: integer;


begin {lastindex}
  if not unpacking and (resultptr^.typ = subranges) then {we have an
                                                         inconveniently-sized
                                                          scalar entity}
    begin
    power2check(len, power2, power2dummy);
    if not power2 then
      begin
      len := len * bitsperunit;
      startunpacking(true, constpart, unpacking);
      end;
    end;
  if unpacking then
    begin
    genlit(constpart);
    oprndstk[sp].oprndlen := len;
    genunary(pindxop, ints);
    end
  else
    begin
    genlit(constpart);
    oprndstk[sp].oprndlen := len;
    genunary(indxop, ints);
    end;
end {lastindex} ;
{>>>}
{<<<}
procedure updatelabelnest;
{ Called when exiting a nested control structure to update maxlegalnest
  field of all labels defined or referenced at the current nesting level.
  The effect is to make all future references at this level to such a
  label illegal.  We also adjust jumpoutnest if a label was referenced
  that has not yet been defined, since it's too late for it to be defined
  in this nesting level.
}
var
  p: labelptr; { used to traverse the list of labels active in this block }

begin
  p := display[level].labellist;
  while p <> labelflag do
    with p^ do
      begin
      if (nest = maxlegalnest) and (definednest = 0) then
        jumpoutnest := min(jumpoutnest, nest - 1);
      if (nest = maxlegalnest) or (nest = definednest) then
        maxlegalnest := nest - 1;
      p := nextlabel;
      end;
end;
{>>>}
{<<<}
procedure statement (follow: tokenset {legal following symbols} );
{ Syntactic routine to parse a statement.
  Productions:
  statement = [ label ":" ] ( empty-statement | assignment-statement |
        procedure-statement | goto-statement | compound-statement |
        if-statement | case-statement | repeat-statement |
        while-statement | for-statement | with-statement  .
  The main statement routine looks at the first token of the statement and
  calls the proper statement-specific routine to parse that statement kind.
}

  {<<<}
    procedure expression(follow: tokenset; {legal following sym}
                         arrayopt: boolean {true if array opt wanted} );
      forward;

  { Parse an expression in which the first factor is possibly already parsed.
    See the body for details.
  }
  {>>>}
  {<<<}
  procedure variable(variantok: boolean; {true if case selector ok}
                     packedok: boolean; {packed ok in context}
                     forindexallowed: boolean; {for index ok in context}
                     newflag: boolean; {newvarop to travrs}
                     parsing: boolean; {true if parsing a variable}
                     varindex: tableIndex);
    forward;
  {>>>}
  {<<<}
  procedure genvalsize(elttype: tableIndex; {element to get size of}
                       eltsize: addressrange {size of fixed element} );
  {<<<}
  { Generate code for the size of a value.  If the type is a conformant
    array parameter, this involves generating code to compute the size.
    If it is an ordinary value, it is just taken from the form entry.

    Note that for a conformant array parameter, the number of elements
    is pushed before the element size.  This allows folding of the
    element size if possible.  The other order allows more common
    expressions, but multiple dimensions on arrays are rare, so this
    is less important.
  }
  {>>>}

  var
    lowid: tableIndex; {index of lowbound for a conformant array}
    et: tableIndex; {element type id}
    es: addressrange; {element size in addressing units}
    f: entryptr; {for access to elementtype}
    packedaccess: boolean; {this is a packed conformant array}

  begin
    packedaccess := false;
    f := ref(bigtable[elttype]);
    with f^ do
      if typ = conformantarrays then
        begin
        lowid := lowbound;
        et := elementtype;
        es := elementsize;
        if packedflag and (es > 0) then
          if es < bitsperunit then
            begin
            packedaccess := true;
            es := bitsperunit div es;
            end
          else
            es := es div bitsperunit;

        variable (true, false, false, false, false, highbound);
        variable (true, false, false, false, false, lowid);

        genbinary (minusop, ints);
        genunary (incop, ints);
        genvalsize (et, es);

        if packedaccess then
          begin
          genbinary (divop, ints);
          genunary (quoop, ints);
          end
        else
          genbinary (mulop, ints);
        end
      else
        pushint (eltsize);
  end;
  {>>>}
  {<<<}
  procedure selector(variantok: boolean; {true if case selector ok}
                     nowvariant: boolean; {true if now a case selector}
                     packedok: boolean; {packed field is ok}
                     var unpacking: boolean; {unpacking a field?}
                     var constpart: addressrange; {const part of addr}
                     var len: addressrange; {operand length}
                     var off: addressrange; {offset for travrs}
                     var varlev: levelindex; {varlev for travrs}
                     var ownvar: boolean {own variable flag for travrs} );
  {<<<}
  { Syntactic routine to parse a selector.

    Productions:

    selector = ( array-selector | field-selector | ptr-selector )
          [* selector *]  .

    array-selector = "[" expression [* "," expression *] "]"  .

    field-selector = "." field-identifier  .

    ptr-selector = "^"  .

    Output generated:

          selector = indexing | field | ptr-ref .

          field = [ "lit(constantpart)" "indxop(len, cost, 'int')" ]  .

          ptr-ref = "lit(constantpart)" "indxop(len, cost, 'int')
                  "varop('sharedPtr^.ptrsize', lev, off, ownvar)" "indrop(len, cost, 'int')"
                  [ "indrop(len, cost, 'int')" ]  .

    Indexing will be described in the routine "onearrayindex".

    This routine parses all variable selectors, and has a few places
    where non-obvious output is generated to make "travrs" work better.
    In particular, the "varop(size, lev, off, ownvar)" includes the "lev",
    "off" and "ownvar" fields entirely to allow travers to invalidate references
    to that variable.  The "off" field is the start of the variable, so that
    for an array reference the entire array is invalidated by a reference
    to any element of the array.

    This routine is concerned with "off" only because pointers and files
    have a fake offset associated with the type, and all references to
    pointers with the same type are invalidated.  As this routine
    generates code to de-reference pointers, it may also have to change
    the "lev" and "off" values to those for the referenced type.

    In addition, files are implemented as pointers to a file control
    table, so buffer variable references have an additional "indrop" added.
  }
  {>>>}

    var
      nowunpacking: boolean; { true if now unpacking }
      indexwanted: tableIndex; { index type, used by onearrayindex}
      arrayelement: tableIndex; { element type, used by onearrayindex}
      p, p1: entryptr; {used for access to name entry}
      fieldindex: tableIndex; {field selected}
      newoff: integer; {new "off" for pointer or file refs}

    {<<<}
    procedure onearrayindex;
    {<<<}
    { Syntactic routine to parse a single array index.   The syntax is
      trivial, it simply parses an expression.  If the next token is a
      comma, it is effectively changed to "]" "[" to keep the parsing
      of the array indexing going.

      The variables "linearize" and "linearfactor" are used to simplify
      the optimization of array indices of the form a[i+1].  If "linearize"
      is set true, then constant terms are added into "linearfactor" so
      they can be included in the constant offset of the array being
      indexed.  "Linearize" is manipulated to be on only when parsing
      an array index.

      "Linearize" and "linearfactor" are local to the routine "statement",
      which is global to all expression parsing routines.  This is because
      they are needed by genbinary, which is outside the "expression" routine,
      and it would be very inconvenient to have to pass them as parameters
      on all calls.  The price paid is some rather hard to understand code
      to manipulate them.

      Output generated:

            indexing = expression [ "lit(min)" "lit(max)"
                    "indxchkop(len, cost, form)" ]
                    ( ( "lit(min)" "minusop(len, cost, 'int')"
                    "paindxop(len, cost, form)" ) |
                    ( "lit(size)" "mulop(len, cost, 'int')"
                    "aindxop(len, cost, form)" ) )  .

      The above is not quite complete, as multiplies and adds may be folded,
      but it gives the right idea.

    }
    {>>>}

    var
      eltsperunit: 0..bitsperunit; {array elts per addressing unit}
      oldlinear: integer; {local save for linearfactor}
      eltsize: integer; {size of an array element}
      alreadyunpacking: boolean; {true if we were unpacking at entry}
      outofbounds: boolean; {set of index out of bounds}
      indextypeptr: entryptr; {for access to index type entry}
      lowerbound: integer; {lower bound of the array index}

      conformant: boolean; {the array is conformant}
      lowid: tableIndex; {lower bound location for a conformant array}
      highid: tableIndex; {upper bound location for a conformant array}
      upperout, lowerout: boolean; {bounds could be out}
      power2: boolean; {multiplier is a power of two}
      power2value: integer; {which power of two it is}
      tmaxautoindex: integer; {temp to hold maxautoindex constant}

    begin
      { The mc68020 has scaling, but the mc68000 does not }
      if sharedPtr^.switcheverplus[cpu68020] then
        tmaxautoindex := maxautoindex
      else
        tmaxautoindex := 0;

      indexwanted := noneindex;
      arrayelement := noneindex;
      eltsize := 1;
      conformant := resultptr^.typ = conformantarrays;
      if resulttype <> noneindex then
        with resultptr^ do
          if (typ in [strings, arrays]) or conformant then
            begin
            arrayelement := elementtype;
            indexwanted := indextype;
            eltsize := elementsize;
            lowid := lowbound;
            highid := highbound;
            end
          else
            warn (arrayexpected);

      gettoken;
      nowunpacking := false;
      alreadyunpacking := unpacking;
      if resultptr^.packedflag then
        if eltsize < bitsperunit then
          nowunpacking := true
        else
          eltsize := eltsize div bitsperunit;
      startunpacking (nowunpacking, constpart, unpacking);

      linearize := (sharedPtr^.switchcounters[indexcheck] <= 0) and not unpacking and not conformant and (varlev <> 0);
      oldlinear := linearfactor;
      linearfactor := 0;
      expression (follow + [comma, rbrack, rpar], linearize);
      if not compatible (indexwanted, resulttype) then
        warnbefore (indexincomp);

      linearize := false;
      oprndstk[sp].typeindex := indexwanted;
      indextypeptr := ref(bigtable[indexwanted]);
      lowerbound := lower (indextypeptr);
      if not conformant and (sharedPtr^.switchcounters[indexcheck] > 0) then
        genunary (indxchkop, ints)
      else
        begin
        checkrange (oprndstk[sp], false, outofbounds, lowerout, upperout);
        if outofbounds then
          warnbefore(indexerror);

        if unpacking or conformant then
          begin
          if conformant then
            begin
            lev := varlev;
            if sharedPtr^.switchcounters[indexcheck] > 0 then
              begin
              genoprnd;
              variable (true, false, false, false, false, lowid);
              genoprnd;
              variable (true, false, false, false, false, highid);
              genunary (cindxchkop, ints);
              end
            else
              begin
              variable (true, false, false, false, false, lowid);
              genbinary (minusop, ints);
              end
            end
          else
            begin
            pushint (lowerbound);
            genbinary (minusop, ints);
            end
          end
        else if abs(constpart + (linearfactor - lowerbound) * eltsize) > maxaddr then
          begin
          pushint (linearfactor - lowerbound);
          genbinary (plusop, ints);
          end
        else constpart := constpart + (linearfactor - lowerbound) * eltsize;
        end;

      linearfactor := oldlinear;
      if token = comma then
        token := lbrack
      else
        verifytoken(rbrack, badindexerr);

      if constcheck(sp) and not conformant then
        begin
        sp := sp - 1;
        with oprndstk[sp + 1], cvalue do
          if unpacking then
            begin
            eltsperunit := bitsperunit div eltsize;
            if not alreadyunpacking then
              begin
              genlit(constpart + intvalue div eltsperunit);
              oprndstk[sp].oprndlen := eltsize;
              genunary (indxop, ints);
              constpart := 0;
              intvalue := intvalue mod eltsperunit;
              end;
            constpart := constpart + intvalue * eltsize;
            end
          else constpart := constpart + intvalue * eltsize;
        end
      else if unpacking then
        begin
        genoprnd;
        oprndstk[sp].oprndlen := eltsize;
        genunary (paindxop, ints);
        end
      else
        begin
        if not conformant then
          power2check(eltsize, power2, power2value)
        else
          power2 := false;
        if power2 and (power2value <= tmaxautoindex) then
          begin
          oprndstk[sp].oprndlen := eltsize;
          oprndstk[sp - 1].oprndlen := eltsize;
          end
        else
          begin
          genvalsize (arrayelement, eltsize);
          genbinary(mulop, ints);
          oprndstk[sp].oprndlen := unitsize;
          oprndstk[sp - 1].oprndlen := unitsize;
          end;
        genbinary (aindxop, ints);
        end;
      newresulttype (arrayelement);
      len := eltsize;
    end;
    {>>>}

    begin {selector}
      repeat
        if resultptr^.packedflag and not packedok then
          begin
          warn(varparamerr);
          packedok := true;
          end;
        p1 := resultptr; {we modify resultptr within "with" statement}
        if token = lbrack then
          begin
          onearrayindex;
          while token = lbrack do onearrayindex;
          nowvariant := false;
          end
        else if token = dot then
          begin
          gettoken;
          with p1^ do
            if token = ident then
              begin
              if typ = fields then
                begin
                searchsection(fieldid, fieldindex);
                if fieldindex <> 0 then
                  begin
                  p := ref(bigtable[fieldindex]);
                  newresulttype(p^.vartype);
                  nowvariant := p^.varianttag;
                  setnowunpacking(packedflag, p^.offset, nowunpacking);
                  if nowunpacking then
                    begin
                    if not unpacking then
                      begin
                      constpart := constpart + p^.offset div (bitsperunit * packingunit) * packingunit;
                      startunpacking(nowunpacking, constpart, unpacking);
                      constpart := constpart + p^.offset mod (bitsperunit * packingunit);
                      end
                    else
                      constpart := constpart + p^.offset;
                    end
                  else if packedflag and not unpacking then
                    constpart := constpart + p^.offset div bitsperunit
                  else constpart := constpart + p^.offset;
                  if packedflag and not unpacking then
                    len := p^.length div bitsperunit
                  else
                    len := sizeof(resultptr, unpacking);
                  end
                else
                  begin
                  warn (undefidenterr);
                  newresulttype (noneindex);
                  end;
                end
              else if typ <> none then
                warnbefore(recordexpected);
              gettoken
              end
            else
              warnbetween (novarerr);
          end
        else if token = uparrow then
          begin
          nowvariant := false;
          with p1^ do
            begin
            if typ = ptrs then
              begin
              p := ref (bigtable[ptrtypename]);
              newresulttype (p^.typeindex);
              newoff := ptrkey;
              end
            else if typ = files then
              begin
              newresulttype (filebasetype);
              newoff := filekey;
              end
            else if typ <> none then
              begin
              warn (ptrexpected);
              newresulttype (noneindex);
              end;

            lastindex (unpacking, constpart, len);
            genop (unsvarop);
            genint (len);
            genint (varlev);
            genint (off);
            genint (ord(ownvar));
            oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
            if sharedPtr^.switchcounters[nilcheck] > 0 then genunary(ptrchkop, ints);
            if (typ = files) then
              begin
              genunary (definelazyop, files);
              genunary (filebufindrop, ints); {can't be a cse}
              end;
            genunary (indrop, ints);
            constpart := 0;
            varlev := 0;
            unpacking := false;
            off := newoff;
            ownvar := false;
            len := sizeof (resultptr, false);
            gettoken
            end;
          end;
      until not (token in [lbrack, dot, uparrow]);
      if nowvariant and not variantok then
        warnnonstandard (novarianttag);
    end {selector} ;
  {>>>}
  {<<<}
  procedure parameterlist(procindex: tableIndex {procedure name entry} ); forward;
  { Syntactic routine to parse a parameter list, see body for details.
  }
  {>>>}
  {<<<}
    procedure illegalident(varindex: tableIndex {index of illegal identifier} );

  { Called when an illegal identifier is found to try to do something sensible
    with the input.  This routine pushes a dummy result to keep the operand
    calculations going, then tries to determine from the next token whether
    it was intended as a variable reference or a procedure reference and
    act accordingly.  There is no worry about the code going out, since code
    emission is turned off by the error.
  }

      var
        {all of the following are dummy parameters}
        len, constpart, dummy1: addressrange;
        unpacking: boolean;
        dummy2: levelindex;


      begin {illegalident}
        pushint(0);
        newresulttype(noneindex);
        oprndstk[sp].operandkind := varoperand;
        oprndstk[sp].typeindex := noneindex;
        oprndstk[sp].cvalue.representation := none;
        if varindex = 0 then warn(undefidenterr)
        else warn(wantvarname);
        gettoken;
        if token in [lbrack, dot, uparrow] then
          selector(true, false, true, unpacking, constpart, len, dummy1, dummy2,
                   unpacking)
        else if token = lpar then parameterlist(0);
        newresulttype(noneindex);
        oprndstk[sp].typeindex := noneindex;
        oprndstk[sp].cvalue.representation := none;
      end {illegalident} ;
  {>>>}
  {<<<}
    procedure illegalassign;

  { Attempt to make some reasonable recovery from a garbled assignment
  }


      begin {illegalassign}
        if token = ident then illegalident(varindex)
        else
          begin
          warn(badassign);
          if token in
             [intconst, realconst, dblrealconst, charconst, stringconst] then
            gettoken;
          end;
        if token in [becomes, eql] then
          begin
          if token = eql then warn(nobecomeserr);
          gettoken;
          expression(follow, false);
          genoprnd;
          end
      end {illegalassign} ;
  {>>>}

  {<<<}
  procedure variable;
                      {variantok: boolean; (true if case selector ok)
                       packedok: boolean; (packed ok in context)
                       forindexallowed: boolean; (for index ok in context)
                       newflag: boolean; (newvarop to travrs)
                       parsing: boolean; (true if parsing a variable)
                       varindex: index}
  {<<<}
  { Syntactic routine to parse a variable reference.
    variable = identifier [ selection ]  .

    This generates the basic code for a variable access, calling on
    "selector" to handle any qualification, indexing, etc.

    This is complicated by the fact that an apparent variable reference
    could be a field within a with, or a function result.  Also, a variable
    could be being used as a for index, in which case it is accessed in a special manner.

    This routine sets the (relatively) global "resulttype" to the type of
    the variable being accessed.
    If "parsing" is false, we are only generating references for an
    existing variable entry, and no parsing will take place.
  }
  {>>>}
  {<<<}
  var
    unpacking: boolean; {we are continuing to unpack}
    nowunpacking: boolean; {either unpacking or should start}
    var_is_valid: boolean; {variable known to contain a legal value}
    ownvar: boolean; {variable is an "own" variable}
    constpart: addressrange; {constant part of var address}
    varlev: levelindex; {level of variable}
    i: forstackindex; {which for index, if it is an index}
    varptr: entryptr; {used to access var name entry}
    off: addressrange; {offset for use by travrs (see selector)}
    len: addressrange; {length of value}
  {>>>}

  begin
    unpacking := false;
    varlev := lev;
    ownvar := false;
    varptr := ref(bigtable[varindex]);
    with varptr^ do
      begin
      if checkforstack (varindex, i) then
        begin
        if not forindexallowed then
          warn(modifiedfor);
        newresulttype (vartype);
        bumpsp;
        with oprndstk[sp] do
          begin
          typeindex := vartype;
          extended := resultptr^.extendedrange;
          oprndlen := length;
          operandkind := varoperand;
          cost := 0;
          setvarrange (sharedPtr^.targetintsize, false, true);
          end;
        genop (forindexop);
        genint (i);
        if parsing then
          gettoken;
        end

      else
        begin {not a for index}
        var_is_valid := false;
        case namekind of
          varname, fieldname, param, varparam, funcparam, procparam,
          confparam, varconfparam, boundid:
            {<<<}
            begin
            var_is_valid := (namekind in [param, boundid]) or knownvalid;
            newresulttype (vartype);
            if varlev > level then
              {<<<  within with}
              with display[varlev] do
                begin
                if withpacking and not packedok then
                  begin
                  warn (varparamerr);
                  packedok := true;
                  end;

                pushdummy;
                genop (withop);
                genint (varlev - level);
                varlev := withlevel;
                off := withoffset;
                setnowunpacking (withpacking, offset, nowunpacking);
                if nowunpacking then
                  begin
                  if not unpacking then
                    begin
                    constpart := offset div (bitsperunit * packingunit) * packingunit;
                    startunpacking(nowunpacking, constpart, unpacking);
                    end;
                  constpart := constpart + offset mod (bitsperunit * packingunit);
                  end
                else if withpacking then
                  constpart := offset div bitsperunit
                else
                  constpart := offset;
                if withpacking and not unpacking then
                  len := length div bitsperunit
                else
                  len := length;
                end
              {>>>}
            else
              {<<<  not within with}
              begin
              constpart := offset;
              if (varlev = level) then
                begin {local variable}
                if not (varalloc in [usealloc, definealloc, sharedalloc]) and
                   not (modified or newflag) and checkundefs and
                   not (anyexternals and (level = 1)) then
                  warn (unassigned);
                end
              else
                begin {not local variable}
                if (varlev > 1) then
                  begin
                  sharedPtr^.proctable[display[level].blockref].intlevelrefs := true;
                  if (varlev > 2) and (varalloc <> absolute) then
                    constpart := constpart + staticlinkoffset;
                  end;
                registercandidate := false;
                if newflag then
                  nestedmod := true;
                end;

              if varalloc = ownalloc then
                begin
                genop (ownop);
                ownvar := true;
                bumpsp;
                with oprndstk[sp] do
                  begin
                  typeindex := intindex;
                  oprndlen := sharedPtr^.ptrsize;
                  extended := false;
                  operandkind := exproperand;
                  cost := 0
                  end;
                end
              else if varalloc in [absolute, usealloc, definealloc, sharedalloc] then
                begin
                varlev := 0;
                newflag := true;
                if varalloc = absolute then {ORIGIN}
                  begin
                  genop (levop);
                  genint (0);
                  genint (0);
                  end
                else
                  begin {USE/DEFINE/SHARED external variable}
                  sharedPtr^.vartable[sparelink div (maxvarentries + 1) + 1]^[sparelink mod (maxvarentries + 1)].referenced := true;
                  genop (extop);
                  genint (sparelink);
                  end;

                bumpsp;
                with oprndstk[sp] do
                  begin
                  typeindex := intindex;
                  oprndlen := sharedPtr^.ptrsize;
                  extended := false;
                  operandkind := varoperand;
                  cost := 0
                  end;
                end
              else
                getlevel (varlev, namekind in [param, varparam, funcparam, procparam, confparam, varconfparam, boundid]);

              len := sizeof (resultptr, unpacking);
              off := constpart;

              { This fixes a 68k problem where a small packed array (of boolean for instance) when moved as a unit, is moved to
                the least significant end instead of the most significant end of the byte that contains it.
                This had some nasty effects in the native compiler itself, including suppressing hoisting!
                The fix is to treat the array as a packed entity }
              if packinghightolow then
                begin
                setnowunpacking (resultptr^.bitaddress, 0, nowunpacking);
                if nowunpacking then
                  begin
                  if not unpacking then
                    startunpacking (nowunpacking, constpart, unpacking);
                  constpart := 0;
                  end;
                len := sizeof (resultptr, unpacking);
                end;
              end;
              {>>>}

            if varalloc = pointeralloc then
              begin
              genlit (constpart);
              genunary (indxop, ints);
              genunary (indrop, ints);
              len := sizeof (resultptr, false); { false because varparams can't be packed element }
              oprndstk[sp].oprndlen := len;
              constpart := 0;
              end;

            if parsing then
              begin
              gettoken;
              if token in [lbrack, dot, uparrow] then
                selector(variantok, varianttag, packedok, unpacking,
                         constpart, len, off, varlev, ownvar);
              end;
            if namekind in [funcparam, procparam] then len := procparamsize;
            end;
            {>>>}
          forwardfunc, funcname:
            {<<<}
            begin
            varlev := varlev + 1;
            getlevel(varlev, false);
            with display[varlev] do
              begin
              constpart := blocksize + paramsize;
              if sharedPtr^.proctable[blockref].externallinkage then
                constpart := constpart + sharedPtr^.extreturnlinksize
              else
                constpart := constpart + sharedPtr^.returnlinksize;
              end;
            {
            Override offset of function value if value must be
            returned in registers.  In that case we allocate the
            function space in the local variables area }
            if sharedPtr^.proctable[display[varlev].blockref].registerfunction <> 0 then
              constpart := 0;
            if (varlev <> level) and (varlev > 1) then
              begin
              sharedPtr^.proctable[display[level].blockref].intlevelrefs := true;
              if varlev > 2 then constpart := constpart + staticlinkoffset;
              end;
            off := constpart;
            newresulttype(functype);
            len := sizeof(resultptr, false);
            if parsing then gettoken;
            end;
            {>>>}
          otherwise
            {<<<}
            begin
            illegalident(varindex);
            if parsing then gettoken;
            end;
            {>>>}
          end;

        setvarrange (len, unpacking, var_is_valid);
        lastindex (unpacking, constpart, len);
        if unsigned (resultptr, len, unpacking) then
          if newflag then
            genop (newunsvarop)
          else
            genop (unsvarop)
        else if newflag then
          genop (newvarop)
        else
          genop (varop);
        genint (len);
        genint (varlev);

        if newflag and (varlev <= 1) then
          with sharedPtr^.proctable[display[level].blockref] do
            begin
            globaldeath := true;
            if display[level].blockref <= cseregions then
              with sharedPtr^.cseregiontable[display[level].blockref, ownvar] do
                begin
                if off < low then
                  low := off;
                if off > high then
                  high := off;
                end;
            end;

        genint (off);
        genint (ord(ownvar));
        oprndstk[sp].oprndlen := sizeof(resultptr, false);
        oprndstk[sp].extended := resultptr^.extendedrange;
        oprndstk[sp].operandkind := varoperand;
        lev := varlev;
        end;
      end;
  end;
  {>>>}
  {<<<}
    procedure modifyvariable (variantok: boolean; {true if case selector ok}
                              packedok: boolean {packed ok in context} );
  { Parse a variable which will be modified.  The actual parsing is done by
    "variable", this routine just does special actions for variables which will be modified. }

      var
        resultindex: tableIndex; {used as temp, and dummy arg}
        p: entryptr; {used for name table access}

      begin {modifyvariable}
        search(resultindex);
        if resultindex = 0 then illegalident(resultindex)
        else
          begin
          p := ref(bigtable[resultindex]);
          with p^ do
            if namekind in
               [varname, fieldname, param, varparam, confparam,
               varconfparam] then
              begin
              modified := true;
              parammodified := true;
              if (nest = 1) and nolabelsofar then knownvalid := true;
              variable(variantok, packedok, false, true, true, resultindex)
              end
            else illegalident(resultindex);
          end
      end {modifyvariable} ;
  {>>>}

  {<<<}
  function filedeclared: boolean;
  { Returns true unless "standard" is specified and token is input or output and is not declared in the program heading }
    var
      fileIndex: tableIndex; {to get to the file name}
      f: entryptr; {to point there once we've found it}

    begin
      filedeclared := true;
      if (sharedPtr^.switchcounters[standard] > 0) and (token = ident) then
        begin
        search (fileIndex);
        if (fileIndex = inputindex) or (fileIndex = outputindex) then
          begin
          f := ref(bigtable[fileIndex]);
          filedeclared := f^.programdecl;
          end;
        end;
    end {filedeclared} ;
  {>>>}
  {<<<}
    procedure fileparam(pushit: boolean; {parameter must be pushed}
                        emitlen: boolean; {element length is needed, too}
                        textfileneeded: boolean {must be a text file} );
  { Parses a file parameter to a procedure.  This is assumed to be the first
    parameter in all cases, as it parses the initial left parenthesis too.
    If "emitlen" is set, the length of a file element in file size units will
    be pushed onto the stack after the file operator. }

      var
        f: entryptr; {for access to file base type}

      begin {fileparam}
        if pushit then
          genop (bldnil);
        verifytoken (lpar, nolparerr);
        if token = ident then
          begin
          if not filedeclared then
            warnnonstandard (filenotdeclared);
          modifyvariable (true, true);
          if not (resultform in [files, none]) then
            warnbefore (nofilevar)
          else
            begin
            if textfileneeded and (resulttype <> textindex) then
              warnbefore (nottextfile);
            if pushit then genunary(pushaddr, resultform);
            if emitlen and (resultform = files) then
              if resulttype = textindex then
                genpushdefaultint ( - 1)
              else
                begin
                f := ref (bigtable[resultptr^.filebasetype]);
                genpushdefaultint (forcealign(sizeof(f, false), alignmentof(f, false),
                                   false) * (bitsperunit div bitsperfileunit));
                end;
            end;
          end
        else
          warnbetween (novarerr);
      end {fileparam} ;
  {>>>}

  {<<<}
    procedure stringtarget;

  { Parse and generate a string target parameter for intrinsic string
    functions.   The argument must be a string variable, and the address
    and length are both passed as parameters.
  }

      var
        varlen: addressrange;

      begin {stringtarget}
        modifyvariable(true, true);
        if not (resultform in [strings, none]) then warnbefore(nostringerr);
        varlen := oprndstk[sp].oprndlen - 1;
        oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
        genunary(pushaddr, strings);
        pushint(varlen);
        oprndstk[sp].oprndlen := shorttargetintsize;
        genunary(pushvalue, ints);
      end {stringtarget} ;
  {>>>}
  {<<<}
    procedure stringsource;

  { Parse a string source parameter for intrinsic string params.  Parameter
    is passed by reference but any string-look-alike is allowed including
    single chars, packed arrays of char, and literal constants.
  }


      begin {stringsource}
        expression(follow + [comma, colon, rpar], false);
        if resultform = chars then genunary(chrstrop, chars)
        else if (resultform = arrays) and resultptr^.stringtype then
          genunary(arraystrop, arrays)
        else if not (resultform in [strings, none]) then warnbefore(nostringerr);
        oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
        genunary(pushaddr, strings);
      end {stringsource} ;
  {>>>}

  {<<<}
    procedure parsecolons(maxcolons: integer {colons allowed in context} );

  { This routine is used for procedure arguments to parse off any write-args,
    even if the procedure is not write.  if the number of colons found exceeds
    "maxcolons", an error message is issued.

    Productions:

    write-parameter = expression [ ":" expression [ ":" expression ] ] .

  }

      var
        coloncount: integer; {count of colons in this writearg}


      begin {parsecolons}
        coloncount := 0;
        if maxcolons > 0 then genoprnd;
        while token = colon do
          begin
          coloncount := coloncount + 1;
          if coloncount > maxcolons then warn(illegalformat);
          gettoken;
          expression(follow + [comma, rpar, colon], false);
          if resultform <> ints then warnbefore(badformat);
          setdefaulttargetintsize;
          genunary(bldfmt, ints); ;
          end
      end {parsecolons} ;
  {>>>}
  {<<<}
    procedure parseextraargs;

  { Syntactic error recovery routine to parse any arguments left after the
    expected number for a standard procedure.  It will accept almost
    anything that looks vaguely like an argument and try to parse it.
    It will issue an error message if it finds anything to do.
  }


      begin {parseextraargs}
        parsecolons(0);
        if token in
           [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar,
           notsym, nilsym] then
          begin
          warn(toomanyargs);
          repeat
            verifytoken(comma, nocommaerr);
            expression(follow + [rpar, comma, colon], false);
            genoprnd;
            parsecolons(0)
          until not (token in
                [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar,
                notsym, nilsym]);
          end;
        verifytoken(rpar, norparerr);
      end {parseextraargs} ;
  {>>>}
  {<<<}
  procedure parameterlist;
  { Syntactic procedure to parse a parameter list.
    Productions:
    actual-parameter-list = "(" actual-parameter
          [* "," actual-parameter *] ")"  .
    Each actual-parameter found is parsed by "oneactualparameter".
    If there are more or fewer parameters found than expected,
    an error is emitted.
  }
  var
    paramcount: integer; {parameters found}
    paramindex: integer; {next formal parameter index}
    maxparams: integer; {max param (end of param index)}
    alreadywarned: boolean; {message already issued, don't duplicate}
    p: entryptr; {used for access to proc entry}
    calltype: tableIndex; {used for function param return type}
    lastconfactual: tableIndex; {type of last actual conformant parameter}

    {<<<}
        procedure oneactualparam;

    { Syntactic routine to parse an actual parameter:

      Productions:

      actual-parameter = expression | variable-access |
            procedure-identifier | function-identifier  .

      The parameter parsed is compared to the corresponding formal parameter
      for compatibility.  Routine parameters have their argument lists
      checked as well.  Parameters are pushed onto the stack in the order
      they are read.
    }

          var
            nextparam: tableIndex; {next formal param for this procedure}
            namekind: nametype; {namekind of the formal parameter}
            actualstdstring: boolean; {true if actual param is a standard string}
            actualindex: tableIndex; {name for actual parameter}
            actualptr: entryptr; {used for access to actual param}
            p: entryptr; {used for access to formal param}
            paramptr: entryptr; {for type of formal param}
            formaltype: tableIndex; {type of formal parameter}
            formallen: addressrange; {size of formal parameter}

    {<<<}
          procedure callparam;

    { A routine parameter has been parsed, so the parameter lists must be
      checked for compatibility, and the routine address and static link
      set up to be passed.  If the actual parameter is itself a procedural
      parameter, the parameter lists are still compared, and the value is
      passed directly.
    }


    {<<<}
            function congruent(actual, lastactual: tableIndex; {limits of actual list}
                               formal,
             lastformal: tableIndex {same for formal list} ): boolean;

    { True if the actual procedure's parameter list (defined by "actual"
      and "lastactual" is congruent to the formal procedure's parameter
      list (defined by "formal" and "lastformal").  "actual" and "formal"
      point to the procedure name, and "lastactual" and "lastformal" are
      the last parameter entries for the procedures.

      Two parameter lists are congruent if each parameter section is of
      the same kind (var, value, procedure, function), has the same number
      of parameters, and the same type.  If the parameter is a conformant
      array, the two schemas must be "equivalent".
    }

              var
                c: boolean; {local value of "congruent"}
                procpar: boolean; {set if procedure parameter}
                ap, fp: entryptr; {access to "actual" and "formal"}
                nexta, nextf: tableIndex; {links to next parameters}


    {<<<}
              function equivtypes(formalt, actualt: tableIndex {type names} ): boolean;

    { True if the types are the same or are equivalent conformal array
      schemas
    }

                var
                  at, ft: entryptr; {for access to type entries}


                begin {equivtypes}
                  equivtypes := false;
                  if identical(formalt, actualt) then equivtypes := true
                  else
                    begin
                    at := ref(bigtable[actualt]);
                    ft := ref(bigtable[formalt]);
                    if (at^.typ = conformantarrays) and
                       (ft^.typ = conformantarrays) then
                      equivtypes := (at^.packedflag = ft^.packedflag) and
                                    identical(at^.indextype, ft^.indextype) and
                                    equivtypes(at^.elementtype, ft^.elementtype);
                    end;
                end {equivtypes} ;
    {>>>}


              begin {congruent}
                c := (lastactual - actual) = (lastformal - formal);
                actual := actual + 1; {point to first parameter}
                formal := formal + 1;
                while c and (actual <= lastactual) do
                  begin
                  ap := ref(bigtable[actual]);
                  fp := ref(bigtable[formal]);
                  nexta := ap^.nextparamlink;
                  nextf := fp^.nextparamlink;
                  c := (ap^.namekind = fp^.namekind) and
                       (ap^.lastinsection = fp^.lastinsection);
                  if c and ap^.lastinsection then
                    begin
                    procpar := ap^.namekind in [procparam, funcparam];
                    c := equivtypes(ap^.vartype, fp^.vartype);
                    if c and procpar then
                      c := congruent(actual, nexta, formal, nextf);
                    end;
                  actual := nexta + 1;
                  formal := nextf + 1;
                  end; {while}
                congruent := c;
              end {congruent} ;
    {>>>}


            begin {callparam}
              if actualptr^.namekind in [procparam, funcparam] then
                begin
                if not congruent(actualindex, actualptr^.nextparamlink, paramindex,
                                 p^.nextparamlink) then
                  warn(paramtypeerr);
                variable(true, false, true, false, true, actualindex);
                oprndstk[sp].oprndlen := procparamsize;
                genunary(pushvalue, arrays);
                end
              else
                begin
                if not congruent(actualindex, actualptr^.paramlist, paramindex,
                                 p^.nextparamlink) or
                   ((sharedPtr^.proctable[actualptr^.procref].calllinkage <>
                   interruptcall) and
                   ((sharedPtr^.proctable[actualptr^.procref].calllinkage <> pascal2call) and
                   not sharedPtr^.switcheverplus[windows])) then
                  warn(paramtypeerr);
                sharedPtr^.proctable[actualptr^.procref].isprocparam := true;
                getlevel(lev, false);
                if (level > 2) and (lev = level) then
                  begin
                  genlit( - staticlinkoffset);
                  genunary(indxop, ints);
                  end;
                if lev > 1 then
                  sharedPtr^.proctable[display[level].blockref].intlevelrefs := true;
                genlit(actualptr^.procref);
                oprndstk[sp].oprndlen := procparamsize;
                genunary(pushproc, resultptr^.typ);
                gettoken
                end;
            end {callparam} ;
    {>>>}
    {<<<}
          function conformable(formal, actual: tableIndex {parameters} ): boolean;

    { True if the actual parameter is conformable to the formal conformant
      array parameter.
    }

            var
              formalptr, actualptr: entryptr; {access to formal and actual}
              formalindex, actualindex: tableIndex; {index types}
              formalelt, actualelt: tableIndex; {element types}
              c: boolean; {partial value of conformable}


            begin {conformable}
              formalptr := ref(bigtable[formal]);
              actualptr := ref(bigtable[actual]);
              if (formalptr^.typ = conformantarrays) and
                 (actualptr^.typ in [conformantarrays, arrays, strings]) then
                begin
                formalindex := formalptr^.indextype;
                actualindex := actualptr^.indextype;
                formalelt := formalptr^.elementtype;
                actualelt := actualptr^.elementtype;
                c := formalptr^.packedflag = actualptr^.packedflag;
                c := c and compatible(formalindex, actualindex) and
                     conformable(formalelt, actualelt);
                if c and (actualptr^.typ in [arrays, strings]) then
                  begin
                  formalptr := ref(bigtable[formalindex]);
                  actualptr := ref(bigtable[actualindex]);
                  c := (lower(actualptr) >= lower(formalptr)) and
                       (upper(actualptr) <= upper(formalptr));
                  end;
                conformable := c;
                end
              else conformable := identical(formal, actual);
            end {conformable} ;
    {>>>}
    {<<<}
          procedure conformantparam(paramtype: tableIndex; {index of parameter description}
                                    pushbounds: boolean; {push the bounds for this one}
                                    valueparam: boolean {this is a value parameter}
                                    );

    { Parse a conformant parameter.  The bounds are passed only for the first
      in a group of conformant parameters.  If the actual parameter is itself
      a conformant parameter, a runtime check on the bounds is required to make
      sure that it will fit in the bounds of the formal parameter.
    }

            var
              paramptr: entryptr; {for access to parameter type data}
              actualptr: entryptr; {for access to actual parameter bounds}
              at1, pt1: tableIndex; {for tracking down lists}
              highid, lowid: tableIndex; {bound id's if conformant}
              actualisconformant: boolean; {actual parameter is also conformant}
              desiredindex: tableIndex; {Index type of formal parameter}
              indexptr: entryptr; {for access to actual index type}
              boundidsize: addressrange; {for determining size of boundid}
              boundidtype: tableIndex; {the subrange used to declare the array}


            procedure pushbound(which: tableIndex);

    { Push a boundid for a conformant parameter
    }


              begin {pushbound}
                variable(true, false, false, false, false, which);
                if resulttype <> desiredindex then
                  begin
                  oprndstk[sp].typeindex := desiredindex;
                  genunary(congruchkop, resultform);
                  end;
              end {pushbound} ;


            begin {conformantparam}
              if oprndstk[sp].oprndlen = 0 then oprndstk[sp].oprndlen := 1
              else if valueparam then genunary(pushcvalue, resultform);

              oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
              genunary(pushaddr, resultform);

              if not identical(resulttype, lastconfactual) then
                warnbefore(confinconsistent);

              if pushbounds then lastconfactual := noneindex
              else lastconfactual := resulttype;
              actualisconformant := resultptr^.typ = conformantarrays;

              if conformable(paramtype, resulttype) then
                begin
                if pushbounds then
                  begin
                  paramptr := ref(bigtable[paramtype]);
                  actualptr := resultptr;
                  while (actualptr^.typ in [arrays, conformantarrays, strings]) and
                        (paramptr^.typ = conformantarrays) do
                    begin
                    paramcount := paramcount + 2;
                    pt1 := paramptr^.elementtype;
                    at1 := actualptr^.elementtype;
                    indexptr := ref(bigtable[paramptr^.lowbound]);
                    boundidsize := indexptr^.length;
                    boundidtype := indexptr^.vartype;
                    highid := actualptr^.highbound;
                    lowid := actualptr^.lowbound;
                    desiredindex := paramptr^.indextype;
                    indexptr := ref(bigtable[actualptr^.indextype]);

                    if actualisconformant then pushbound(lowid)
                    else pushint(lower(indexptr));
                    oprndstk[sp].typeindex := boundidtype;
                    oprndstk[sp].oprndlen := boundidsize;
                    genunary(pushvalue, getform(indexptr));
                    genoprnd;

                    if actualisconformant then pushbound(highid)
                    else pushint(upper(indexptr));
                    oprndstk[sp].typeindex := boundidtype;
                    oprndstk[sp].oprndlen := boundidsize;
                    genunary(pushvalue, getform(indexptr));
                    genoprnd;

                    paramptr := ref(bigtable[pt1]);
                    actualptr := ref(bigtable[at1]);
                    end;
                  end;
                end
              else warnbefore(paramtypeerr);

              newresulttype(lastconfactual);
              oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
            end {conformantparam} ;
    {>>>}


          begin {oneactualparam}
            paramcount := paramcount + 1;
            newresulttype(noneindex);
            if (paramindex > maxparams) then
              begin
              if not alreadywarned then
                begin
                warn(toomanyargs);
                alreadywarned := true
                end;
              paramindex := maxparams
              end;
            p := ref(bigtable[paramindex]);
            namekind := p^.namekind;
            nextparam := p^.nextparamlink + 1;
            if alreadywarned or (namekind in [param, confparam]) or
               (paramindex = 0) then
              begin
              expression(follow + [comma, colon, rpar], false);
              actualstdstring := (resultform = arrays) and resultptr^.stringtype;
              if not alreadywarned then
                begin
                p := ref(bigtable[paramindex]);
                formaltype := p^.vartype;
                paramptr := ref(bigtable[formaltype]);
                formallen := p^.length;
                if namekind = param then
                  begin
                  if (resultform = ints) and (getform(paramptr) = reals) then
                    begin
                    newresulttype(realindex);
                    genunary(float, ints);
                    end
                  else if (resultform = ints) and (getform(paramptr) = doubles) then
                    begin
                    newresulttype(doubleindex);
                    genunary(float_double, ints);
                    end
                  else if (resultform = reals) and
                          (getform(paramptr) = doubles) then
                    begin
                    newresulttype(doubleindex);
                    oprndstk[sp].oprndlen := doublesize;
                    genunary(real_to_dbl, reals);
                    end
                  else if (getform(paramptr) = strings) and
                          (resultform = chars) then
                    genunary(chrstrop, strings)
                  else if (getform(paramptr) = strings) and actualstdstring then
                    genunary(arraystrop, strings)
                  else
                    begin
                    if (sharedPtr^.switchcounters[standard] <= 0) and
                       (getform(paramptr) = arrays) and paramptr^.stringtype and
                       (resultform = chars) then
                      begin
                      newstringtype(resulttype, arrays, 1);
                      newresulttype(resulttype);
                      oprndstk[sp].typeindex := resulttype;
                      end;
                    if not compatible(resulttype, formaltype) then
                      warnbefore(paramtypeerr);
                    end;
                  gencheck(rangechkop, formaltype);
                  end
                else if (namekind = confparam) and
                        (resultform = conformantarrays) then
                  warnbefore(badconfactual);
                if namekind = param then
                  if p^.varalloc = pointeralloc then
                    begin
                    if p^.modified and
                       (oprndstk[sp].operandkind <> exproperand) then
                      genunary(pushcvalue, resultform);
                    oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
                    genunary(pushaddr, resultform);
                    end
                  else
                    begin
                    oprndstk[sp].oprndlen := formallen;
                    genunary(pushvalue, resultform);
                    end
                else conformantparam(p^.vartype, p^.lastinsection, true);
                end;
              end
            else if token = ident then
              begin
              search(actualindex);
              if actualindex = 0 then illegalident(actualindex)
              else
                begin
                actualptr := ref(bigtable[actualindex]);
                case namekind of
                  varparam:
                    begin
                    modifyvariable(false, false);
                    p := ref(bigtable[paramindex]);
                    if not (p^.univparam or identical(p^.vartype, resulttype)) then
                      warnbefore(paramtypeerr);
                    oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
                    genunary(pushaddr, resultform);
                    if token in begexprset then warn(varparamerr);
                    end;
                  procparam:
                    if not (actualptr^.namekind in
                       [forwardproc, externalproc, procname, procparam]) then
                      begin
                      warn(badprocparam);
                      illegalident(actualindex)
                      end
                    else callparam;
                  funcparam:
                    if not (actualptr^.namekind in
                       [forwardfunc, externalfunc, funcname, funcparam]) then
                      begin
                      warn(badfuncparam);
                      illegalident(actualindex)
                      end
                    else
                      begin
                      newresulttype(p^.vartype);
                      if actualptr^.namekind = funcparam then
                        calltype := actualptr^.vartype
                      else calltype := actualptr^.functype;
                      if not identical(resulttype, calltype) then
                        warn(paramtypeerr);
                      callparam;
                      end;
                  varconfparam:
                    begin
                    modifyvariable(true, false);
                    conformantparam(p^.vartype, p^.lastinsection, false);
                    if token in begexprset then warn(varparamerr);
                    end;
                  end;
                end
              end
            else warn(novarerr);
            paramindex := nextparam;
            parsecolons(0);
            genoprnd;
            verify1([rpar, comma] + follow, badparamerr);
          end {oneactualparam} ;
    {>>>}

  begin {parameterlist}
    lastconfactual := noneindex;
    alreadywarned := false;
    p := ref(bigtable[procindex]);
    with p^ do
      if (procindex = 0) or
         not (namekind in
         [funcparam, procparam, externalfunc, procname, funcname, externalproc, forwardfunc,
         forwardproc]) then
        begin
        maxparams := procindex;
        alreadywarned := true;
        end
      else if (namekind in [funcparam, procparam]) then
        maxparams := nextparamlink
      else maxparams := paramlist;
    paramcount := 0;
    paramindex := procindex + 1;
    if token = lpar then
      begin
      gettoken;
      oneactualparam;
      while token = comma do
        begin
        gettoken;
        oneactualparam;
        end;
      verifytoken(rpar, norparerr);
      end;
    if (paramindex <= maxparams) and not alreadywarned then
      warnbefore(toofewargs);
    genlit(paramcount);
  end {parameterlist} ;
  {>>>}
  {<<<}
    procedure procedurecall(where: tableIndex {proc name index} );

  { Syntactic routine to parse a procedure call:

    Productions:

    procedure-statement = identifer [ actual-parameter-list ]  .

    On parsing the call space is saved for the function value (if any),
    the parameters are put on the stack by "parameterlist", and the call
    is generated.

    This routine also book-keeps any cross-level references needed
  }

      var
        ftype: tableIndex; {function type}
        plen: addressrange; {function result size}
        bref: proctableindex; {current procedure ref index}
        pref: proctableindex; {procedure ref index}
        p: entryptr; {provides access to procedure name/type entry}
        l: levelindex; {calling proc level}


      begin {procedurecall}
        gettoken;
        p := ref(bigtable[where]);
        with p^ do
          begin
          ftype := functype;
          pref := procref;
          end;
        p := ref(bigtable[ftype]);
        plen := sizeof(p, false);
        genlit(pref);
        genop(reserve);
        genint(plen);
        bref := display[level].blockref;
        l := level;
        with sharedPtr^.proctable[pref] do
          begin
          if (l - lev > levelspread) and (lev <> 1) then levelspread := l - lev;
          if (lev <> 1) and (not bodydefined or intlevelrefs) and (l > 1) then
            sharedPtr^.proctable[bref].intlevelrefs := true;
          if globaldeath or not bodydefined then
            begin
            sharedPtr^.proctable[bref].globaldeath := true;
            if (bref <= cseregions) and (pref <= cseregions) then
              begin
              with sharedPtr^.cseregiontable[bref, false] do
                begin
                if sharedPtr^.cseregiontable[pref, false].low < low then
                  low := sharedPtr^.cseregiontable[pref, false].low;
                if sharedPtr^.cseregiontable[pref, false].high > high then
                  high := sharedPtr^.cseregiontable[pref, false].high;
                end;
              with sharedPtr^.cseregiontable[bref, true] do
                begin
                if sharedPtr^.cseregiontable[pref, true].low < low then
                  low := sharedPtr^.cseregiontable[pref, true].low;
                if sharedPtr^.cseregiontable[pref, true].high > high then
                  high := sharedPtr^.cseregiontable[pref, true].high;
                end;
              end;
            end;
          if calllinkage <> pascal2call then anynonpascalcalls := true;

          { Interrupt linkage routines may not be called.
          }
          if calllinkage = interruptcall then warn(badinterruptproc);
          end;
        parameterlist(where);
        newresulttype(ftype);
        pushdummy;
        oprndstk[sp].typeindex := resulttype;
        oprndstk[sp].oprndlen := plen;
        setvarrange(plen, false, false);
        p := ref(bigtable[ftype]);
        if unsigned(p, plen, false) then genunary(unscall, resultform)
        else genunary(call, resultform);
      end {procedurecall} ;
  {>>>}
  {<<<}
    procedure paramcall(where: tableIndex {procedure name index} );

  { Syntactic routine to parse a parameter procedure call.  The
    syntax is the same as for an ordinary procedure call, but the
    actual call is different.
  }

      var
        ftype: tableIndex; {function type pointer}
        plen: addressrange; {function result size}
        p: entryptr; {provides access to procedure type entry}


      begin {paramcall}
        variable(true, false, true, false, true, where);
        ftype := resulttype;
        p := ref(bigtable[ftype]);
        plen := sizeof(p, false);
        genop(reserve);
        genint(plen);
        parameterlist(where);
        newresulttype(ftype);
        oprndstk[sp].oprndlen := plen;
        setvarrange(oprndstk[sp].oprndlen, false, false);
        if unsigned(resultptr, plen, false) then
          genunary(unscallparam, resultform)
        else genunary(callparam, resultform);
        with sharedPtr^.proctable[display[level].blockref] do
          begin
          globaldeath := true;
          intlevelrefs := true;
          if display[level].blockref <= cseregions then
            begin
            with sharedPtr^.cseregiontable[display[level].blockref, false] do
              begin
              low := 0;
              high := maxaddr;
              end;
            with sharedPtr^.cseregiontable[display[level].blockref, true] do
              begin
              low := 0;
              high := maxaddr;
              end;
            end;
          end;
      end {paramcall} ;
  {>>>}
  {<<<}
    procedure parsetagparams(var currentrecord: tableIndex {starting point} );

  { Parse a stream of tag variants, updating currentrecord as we march
    along.  Used by "new", "dispose", and "sizeof".
  }

      var
        currentptr: entryptr; {points to currentrecord}
        tagtype: tableIndex; {index of currentrecord's tag, if any}
        f, p: entryptr; {the better to point with...}
        labvalue: operand; {the given tag labels}


      begin {parsetagparams}
        while token in
              [comma, ident, nilsym, lpar, plus, minus, intconst..stringconst] do
          begin
          verifytoken(comma, nocommaerr);
          constant([comma, rpar], true, labvalue);
          currentptr := ref(bigtable[currentrecord]);
          if getform(currentptr) = fields then
            begin
            tagtype := noneindex;
            if currentptr^.tagfield <> 0 then
              begin
              p := ref(bigtable[currentptr^.tagfield]);
              tagtype := p^.vartype
              end
            else if currentptr^.firstvariant <> 0 then
              begin
              f := ref(bigtable[currentptr^.firstvariant]);
              if f^.firstlabel <> 0 then
                begin
                f := ref(bigtable[f^.firstlabel]);
                tagtype := f^.varlabtype;
                end;
              end;
            if not compatible(labvalue.typeindex, tagtype) then
              warnbefore(badcaselab);
            searchvariants(currentrecord, labvalue);
            end
          else if currentrecord <> noneindex then
            begin
            warnbefore(nofieldtype);
            currentrecord := noneindex
            end;
          end;
      end {parsetagparams} ;
  {>>>}
  {<<<}
  procedure factor;
  { Syntactic routine to parse a factor.
    factor = variable-access | unsigned-constant | function-designator |
          set-constructor | "(" expression ") | "not" factor |
          "@" variable-access  .
    As the factor is parsed, intermediate output is generated to compute it.
    The global "resulttype" is set to the type of the factor.
  }

  var
    unpacking: boolean; {true if unpacking a record or array}
    setisconst: boolean; {true if [setexpression] is constant}
    varindex: tableIndex; {index of var if found}
    varptr, p: entryptr; {provides access to var nameentry if needed}
    constpart, constantlen, off: addressrange;
    {"selector" args for const struct}
    ownvar: boolean; {false as only const structs are handled here}
    varlev: levelindex; {dummy arg to "selector"}
    setelementtype: tableIndex; {elt type for set constructor}
    settype: tableIndex; {created set type for set constructor}
    setptr: entryptr; {used to access settype}
    setentry: tableentry; {local copy of settype entry}
    baseptr: entryptr; {used to access base type of set}

    {<<<}
    procedure standardfunctions(procid: standardids {std func no.} );
    { Parse and generate standard functions.  Syntax varies slightly, but
      is different for different functions.
    }
    var
      paramform: types; {type of the parameter}

      {<<<}
      procedure beginparams;
      {<<<}
      { Parse a single parameter. generating intermediate code for the
        expression, and push the stack for the result
      }
      {>>>}

        begin {beginparams}
          verifytoken(lpar, nolparerr);
          expression(follow + [rpar, comma, colon], false);
        end {beginparams} ;
      {>>>}
      {<<<}
      procedure stringexpr;
      { Parse one expression for concat, and convert to string if necessary }


        begin {stringexpr}
          expression(follow + [comma, rpar], false);
          if resultform = chars then genunary(chrstrop, strings)
          else if (resultform = arrays) and resultptr^.stringtype then
            genunary(arraystrop, strings)
          else if resultform <> strings then warnbefore(paramtypeerr);
        end {stringexpr} ;
      {>>>}
      {<<<}
      procedure finishparams(legaltypes: typeset; {types allowed}
                             newtype: tableIndex {function result type} );
      { Set up the returned value with the result type, and parse any extra
        arguments.  On entry to this procedure, "resulttype" is set to the
        type of the operand by the expression parser.  This is changed as
        necessary to "newtype".
      }

        begin {finishparams}
          if not (resultform in legaltypes) then
            begin
            warnbefore(badfunctionarg);
            newresulttype(noneindex);
            end
          else newresulttype(newtype);
          paramform := resultform;
          oprndstk[sp].typeindex := newtype;
          oprndstk[sp].oprndlen := sizeof(resultptr, false);
          parseextraargs;
        end {finishparams} ;
      {>>>}
      {<<<}
      procedure time;
      { Process the standard routine "time". }

        begin {time}
          pushdummy;
          newresulttype(realindex);
          paramform := reals;
          oprndstk[sp].oprndlen := sharedPtr^.targetrealsize;
          oprndstk[sp].typeindex := realindex;
          genop(bldnil);
        end {time} ;
      {>>>}
      {<<<}
      procedure iofunction(finaltype: tableIndex {type of value returned} );
      { Parse the file functions "ioerror" and "iostatus" }

        begin {iofunction}
          fileparam(false, false, false);
          finishparams([none, files], finaltype);
          if finaltype = intindex then setdefaulttargetintsize;
        end {iofunction} ;
      {>>>}
      {<<<}
      procedure transcendentals;
      { Parse the transcendental functions.  This may require that an integer argument be converted to real }

        begin {transcendentals}
          beginparams;
          if resultform = ints then
            begin
            newresulttype(realindex);
            genunary(float, ints)
            end;
          finishparams([none, reals, doubles], resulttype);
        end {transcendentals} ;
      {>>>}
      {<<<}
      procedure sincosfn;

      { Handle the sin and cos functions.  They are complicated by the
        68881 fsincos instruction that returns both the sin and cos.
      }


        begin {sincosfn}
          if sharedPtr^.switcheverplus[fpc68881] then
            begin
            genlit(ord(fsincos2id));
            transcendentals;
            genunary(sysfn, paramform);
            end
          else
            transcendentals
        end {sincosfn} ;
      {>>>}
      {<<<}
      procedure oddfunction;
      { Process the odd function.  No special action }

        begin {oddfunction}
          beginparams;
          finishparams([ints, none], boolindex);
          setvarrange(unitsize, false, true);
        end {oddfunction} ;
      {>>>}
      {<<<}
      procedure absfunction;
      { Process an "abs" function.  The result is of the same type as the input }

        {<<<}
        procedure absrange(operand: range; {operand having abs taken}
                           var result: range {resulting range} );
        { Compute resulting range for the "abs" function }

        var
          temp: integer; {used to negate values}
          ov: boolean; {dummy overflow flag}

        begin {absrange}
          result := operand;
          with operand do
            if not result.extended and (minlimit < 0) then
              begin
              negate (minlimit, temp, ov);
              result.maxlimit := max (temp, maxlimit);
              if (maxlimit < 0) then
                negate (maxlimit, result.minlimit, ov)
              else
                result.minlimit := 0;
              end;
        end;
        {>>>}

        begin {absfunction}
          beginparams;
          with oprndstk[sp] do
            if resultform = ints then
              begin
              absrange(value_range.optimistic, value_range.optimistic);
              absrange(value_range.pessimistic, value_range.pessimistic);
              end;
          finishparams([none, ints, reals, doubles], resulttype);
        end {absfunction} ;
      {>>>}
      {<<<}
      procedure sqrfunction;
      { Process a "sqr" function.  The result is of the same type as the input }

        {<<<}
        procedure sqrrange(operand: range; {operand being squared}
                           procedure op(left, right: integer;
                                        var result: integer;
                                        var overflow: boolean);
                           var result: range {resulting range} );
        { Compute resulting range for the "sqr" function }

        var
          temp: integer; {used to square values}
          ov: boolean; {dummy overflow flag}

        begin
          with operand do
            begin
            op (maxlimit, maxlimit, result.maxlimit, ov);
            op (minlimit, minlimit, temp, ov);

            result.maxlimit := max (temp, result.maxlimit);
            if (minlimit < 0) then
              result.minlimit := 0
            else
              result.minlimit := temp;

            result.extended := extended;
            end;
        end;
        {>>>}

        begin {sqrfunction}
          beginparams;
          with oprndstk[sp] do
            if resultform = ints then
              if extended then
                begin
                sqrrange(value_range.optimistic, usmultiply,
                         value_range.optimistic);
                sqrrange(value_range.pessimistic, usmultiply,
                         value_range.pessimistic);
                end
              else
                begin
                sqrrange(value_range.optimistic, multiply,
                         value_range.optimistic);
                sqrrange(value_range.pessimistic, multiply,
                         value_range.pessimistic);
                end;
          finishparams([none, ints, reals, doubles], resulttype);
        end {sqrfunction} ;
      {>>>}
      {<<<}
      procedure truncround;
      { Process a trunc or round function.  No special processing }

        begin {truncround}
          beginparams;
          finishparams([none, reals, doubles], intindex);
          setdefaulttargetintsize;
          setvarrange(defaulttargetintsize, false, true);
        end {truncround} ;
      {>>>}
      {<<<}
      procedure ordfunction;
      { Parse an ord function.  No special action here, but no "sysfn" is generated, just the operand }

        begin {ordfunction}
          beginparams;
          finishparams([none, ints, bools, chars, scalars], intindex);
        end {ordfunction} ;
      {>>>}
      {<<<}
      procedure chrfunction;
      { Parse a chr function.  Similar to ord, but leaves type char as the result }

        begin {chrfunction}
          beginparams;
          finishparams([none, ints], chartypeindex);
        end {chrfunction} ;
      {>>>}
      {<<<}
      procedure succpred;
      { Parse a succ or pred function.  No special action }

        begin {succpred}
          beginparams;
          pushint(1);
          if procid = succid then genbinary(plusop, ints)
          else genbinary(minusop, ints);
          finishparams([none, scalars, ints, chars, bools], resulttype);
        end {succpred} ;
      {>>>}
      {<<<}
      procedure lengthfunction;
      { Parse length function.  This returns the length of a string expression.
        Since the string length is stored as the first byte of a string, it is
        exactly equivalent to ord(stringexpr[0]).
      }
        var
          oldcheckundefs: boolean; {save/restore checkundefs flag}

          {<<<}
          procedure setlength(l: addressrange);
           { Set length to the constant value l }

            begin {setlength}
              genunary(deleteop, none);
              parseextraargs;
              sp := sp - 1;
              pushint(l);
              resultform := ints;
              resulttype := intindex;
            end {setlength} ;
          {>>>}

      begin
        verifytoken (lpar, nolparerr);
        oldcheckundefs := checkundefs;
        checkundefs := false;

        expression (follow + [comma, rpar], false);
        checkundefs := oldcheckundefs;

        if resultform = chars then
          setlength (1)
        else if (resultform = arrays) and resultptr^.stringtype then
          setlength (resultptr^.arraymembers)
        else
          begin
          finishparams([none, strings], intindex);
          with oprndstk[sp] do
            begin
            oprndlen := charsize;
            value_range.optimistic.maxlimit := 255;
            value_range.pessimistic.maxlimit := 255;
            end;

          genop (unsvarop);
          genint (charsize);
          genint (0);
          genint (0);
          genint (0);
          end;
      end;
      {>>>}
      {<<<}
      procedure eofeoln;
      { Parse a file function.  This has a default argument of the file "input" if no argument is provided }

      begin
        if token <> lpar then
          begin
          getlevel(1, false);
          genlit (sharedPtr^.inputoffset);
          genunary (indxop, ints);
          genop (varop);
          genint (sharedPtr^.ptrsize);
          genint (1);
          genint (sharedPtr^.inputoffset);
          genint (0);
          oprndstk[sp].typeindex := textindex;
          oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
          paramform := bools;
          if sharedPtr^.switchcounters[nilcheck] > 0 then
            genunary (ptrchkop, ints);
          genunary (definelazyop, files);
          if not inputdeclared then
            warnnonstandard (inputnotdeclared);
          end
        else
          begin
          beginparams;

          if not (resultform in [none, files]) then
            warnbefore (nofilevar)
          else if (procid = eolnid) and (resulttype <> textindex) then
            warnbefore (nottextfile);

          if resultform = files then
            begin
            if sharedPtr^.switchcounters[nilcheck] > 0 then
              genunary (ptrchkop, ints);
            genunary (definelazyop, files);
            end;

          finishparams ([none, files], boolindex);
          end;

        newresulttype (boolindex);
        setvarrange (unitsize, false, true);
      end;
      {>>>}
      {<<<}
      procedure firstypeparam (varok: boolean; {true allows type name or var}
                               var restype: tableIndex {type found} );
      { Parse a type parameter for the functions "size" and "loophole". }

      var
        typeident: tableIndex; {name block for type ident}
        p: entryptr; {used to access type name block}
        oldemitflag: boolean; {so we can turn of intcode emission}
        oldcheck: boolean; {and the unassigned value check}

      begin
        verifytoken (lpar, nolparerr);
        restype := noneindex;
        if token = ident then
          begin
          search (typeident);
          if typeident = 0 then
            warn (undefidenterr)
          else
            begin
            p := ref(bigtable[typeident]);
            if p^.namekind in [typename, undeftypename] then
              begin
              restype := p^.typeindex;
              gettoken;
              end
            else if p^.namekind in [varname, fieldname, param, varparam, boundid] then
              begin
              oldemitflag := emitflag;
              oldcheck := checkundefs;
              checkundefs := false;
              emitflag := false;
              variable (true, false, true, false, true, typeident);
              sp := sp - 1;
              emitflag := oldemitflag;
              checkundefs := oldcheck;
              restype := resulttype;
              end

            else
              begin
              warn (notypenameerr);
              gettoken;
              end;
            end;
          end
        else
          verifytoken (ident, notypenameerr);
      end;
      {>>>}
      {<<<}
      procedure sizefunction;
      { Process the size and bitsize functions.  These always return a
        constant which is the size of the type identifier used as a pseudo-
        parameter.
      }
      var
        thistype: tableIndex;
        f: entryptr; {access to thistype}

      begin
        firstypeparam (true, thistype);
        parsetagparams (thistype);

        f := ref(bigtable[thistype]);
        pushint (sizeof(f, procid = bitsizeid));

        with oprndstk[sp] do
          begin
          extended := cvalue.intvalue < 0;
          setconstrange (cvalue.intvalue, false, value_range);
          end;

        newresulttype (intindex);
        parseextraargs;
      end;
      {>>>}
      {<<<}
      procedure lowerupperfunction;
      { Process the lower and upper functions, returning the first value of a given type, or of an expression }

      var
        thistype: tableIndex; {index to the resulting type}
        f: entryptr; {and pointer to same}
        setflag: boolean; {set true if upper/lower of a set}

      begin {lowerupperfunction}
        setflag := false;
        firstypeparam(true, thistype);
        f := ref(bigtable[thistype]);
        if f^.typ in [sets, arrays] then
          begin {royal kludge per customer request}
          if f^.typ = arrays then
            thistype := f^.indextype
          else
            begin
            setflag := true;
            thistype := f^.basetype;
            end;
          f := ref(bigtable[thistype]);
          end;

        if not (f^.typ in [none, bools, scalars, ints, subranges, chars]) then
          warnbefore(badfunctionarg);
        if procid = lowerid then
          if setflag then
            pushint(max(0, lower(f)))
          else
            pushint(lower(f))
        else if setflag then
          pushint(min(maxsetord, upper(f)))
        else
          pushint(upper(f));

        with oprndstk[sp] do
          begin
          typeindex := thistype;
          oprndlen := min (sharedPtr^.targetintsize, sizeof(f, false));
          setconstrange(cvalue.intvalue, false, value_range);
          end;

        newresulttype (thistype);
        parseextraargs;
      end {lowerupperfunction} ;
      {>>>}
      {<<<}
      procedure loopholefunction;
      { Implement the "loophole" function.  This is a general type transfer
        function that changes the type of a variable, though it leaves the representation alone.
      }

      var
        newtype: tableIndex;
        newptr: entryptr;
        newform: types;
        newsize: addressrange;
        newunsigned: boolean;

      begin
        firstypeparam (false, newtype);
        newptr := ref(bigtable[newtype]);
        newsize := sizeof(newptr, false);
        newform := getform(newptr);
        newunsigned := unsigned (newptr, newsize, false);

        verifytoken (comma, nocommaerr);
        expression (follow + [comma, rpar, colon], false);
        genoprnd;

        sp := sp + 1;
        oprndstk[sp].operandkind := exproperand;
        oprndstk[sp].cost := 0;

        if not ((newform in [none, ints, bools, chars, scalars, ptrs]) and
           (resultform in [none, ints, bools, chars, scalars, ptrs]) or
           (newsize = sizeof(resultptr, false))) then
          warnbefore (typesincomp);

        finishparams ([resultform], newtype);
        setvarrange (newsize, false, false);
        genlit (ord(newunsigned));
        genunary (loopholeop, newform);
      end;
      {>>>}
      {<<<}
      procedure reffunction;
      { Implement the 'ref' function, which builds a pointer to an item.
        A special hack (aren't all hacks special?) enables this new pointer
        type to bypass normal type checking, which requires two pointers to
        be identical to be type compatible.  Since we are building an anonymous
        type, it will not be identical to anything.  Thus the hack, which involves
        the setting of a special bit in the form 'refdefined'.
      }

      var
        varindex: tableIndex;
        varptr: entryptr;

      begin
        verifytoken (lpar, nolparerr);
        if token = ident then
          begin
          search (varindex);
          varptr := ref(bigtable[varindex]);
          modifyvariable (true, false);
          end
        else
          begin
          warnbetween (novarerr);
          gettoken;
          pushint (0);
          end;

        oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
        genunary (addrop, ptrs);
        if tabletop = tablesize then
          analysFatal (tablefull)
        else
          tabletop := tabletop + 1;

        varptr := ref(bigtable[tabletop]);
        with varptr^ do
          begin
          dbgsymbol := 0;
          form := false;
          namekind := typename;
          typeindex := resulttype;
          refdefined := true;
          charindex := 0;
          charlen := 0;
          end;
        enterform (ptrs, resulttype, resultptr);

        with resultptr^ do
          begin
          ptrtypename := tabletop - 1;
          ptrkey := lastfilekey;
          lastfilekey := lastfilekey - 1;
          size := sharedPtr^.ptrsize;
          align := ptralign;
          end;
        oprndstk[sp].typeindex := resulttype;
        verifytoken (rpar, norparerr);
        newresulttype (resulttype);
      end;
      {>>>}
      {<<<}
      procedure copy;
      { Parse "copy(stringsource, pos, num)".  Why isn't this called "substring"? Ask Borland someday! }

      var
        strlen: addressrange;

      begin
        genlit(0);
        genop(reserve);
        genint(maxstrlen + 1);

        verifytoken (lpar, nolparerr);
        expression (follow + [comma, colon, rpar], false);

        if resultform = chars then
          genunary (chrstrop, chars)
        else if (resultform = arrays) and resultptr^.stringtype then
          genunary (arraystrop, arrays)
        else if not (resultform in [strings, none]) then
          warnbefore (nostringerr);

        strlen := oprndstk[sp].oprndlen;
        oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
        genunary (pushaddr, strings);
        verifytoken (comma, nocommaerr);

        expression(follow + [comma], false);
        if not (resultform in [none, ints]) then
          warn (badfunctionarg);

        setshorttargetintsize;
        genunary (pushvalue, ints);
        verifytoken (comma, nocommaerr);
        expression (follow + [rpar], false);
        if not (resultform in [none, ints]) then
          warn (badfunctionarg);

        setshorttargetintsize;
        genunary (pushvalue, ints);
        parseextraargs;
        newstringtype (resulttype, strings, strlen);
        newresulttype (resulttype);
        paramform := strings;
        { Must remove the parameters from the evaluation stack, since copy is a
          function.  Genunary expects a description of the copy operator on the
          stack, not the first parameter, so this hack is only partly correct. }
        sp := sp - 2;
        oprndstk[sp].oprndlen := strlen;
      end;
      {>>>}
      {<<<}
      procedure concat;
      { Parse concat(s1,s2,...sn) function.  Any number of arguments are allowed but must be strings! }

      begin {concat}
        verifytoken (lpar, nolparerr);
        stringexpr;

        repeat
          verifytoken (comma, nocommaerr);
          stringexpr;
          genbinary (plusop, strings);
        until not (token in follow + [comma]);

        verifytoken (rpar, norparerr);
      end;
      {>>>}
      {<<<}
      procedure pos;
      { Parse "pos(string1, string2)" }

      begin
        genlit (0);
        genop (reserve);
        genint (shorttargetintsize);
        verifytoken (lpar, nolparerr);

        stringsource;
        verifytoken (comma, nocommaerr);

        stringsource;
        verifytoken (rpar, norparerr);

        newresulttype (intindex);

        { Must remove the parameters from the evaluation stack, since pos is a
          function.  Genunary expects a description of the pos operator on the
          stack, not the parameter, so this hack is only partly correct. }
        sp := sp - 1;
        oprndstk[sp].oprndlen := shorttargetintsize;
        paramform := strings;
      end;
      {>>>}
      {<<<}
      procedure snglfn;
      { Convert double to single }


        begin {snglfn}
          if sharedPtr^.switcheverplus[doublereals] then warnbefore(badcvtfunc);

          beginparams;

          if resultform = ints then
            begin
            newresulttype(realindex);
            finishparams([none, reals], realindex);
            genunary(float, ints);
            end
          else
            begin
            finishparams([none, doubles], realindex);
            genunary(dbl_to_real, doubles);
            end;
        end; {snglfn}
      {>>>}
      {<<<}
      procedure dblfn;
      { Convert single to double }


        begin {dblfn}
          if sharedPtr^.switcheverplus[doublereals] then warnbefore(badcvtfunc);

          beginparams;

          if resultform = ints then
            begin
            newresulttype(doubleindex);
            finishparams([none, doubles], doubleindex);
            genunary(float_double, ints);
            end
          else
            begin
            finishparams([none, reals], doubleindex);
            genunary(real_to_dbl, reals);
            end;
        end; {dblfn}
      {>>>}
      {<<<}
      procedure mc68881_realfn1(procid: standardids {func no.} );
      { Process a special 68881 inline function that has one real or double
        argument.  An integer argument is converted to real.
      }

      begin
        if sharedPtr^.switcheverplus[fpc68881] then
          begin
          beginparams;
          finishparams([none, reals, doubles], resulttype);
          end
        else
          illegalident (varindex)
      end;
      {>>>}
      {<<<}
      procedure mc68881_realfn2(procid: standardids {func no.} );
      { Process a special 68881 inline function that has two real or double
        arguments.  If the two arguments are in the set real, double or
        integer, but are not the same type, the arg(s) will be converted
        according to the same rules used for assignments.
      }

      var
        firstform: types; {form of first argument}

      begin
        if sharedPtr^.switcheverplus[fpc68881] then
          begin
          verifytoken(lpar, nolparerr);
          expression(follow + [comma], false);

          if resultform = ints then
            begin
            newresulttype(realindex);
            genunary(float, ints)
            end;

          firstform := resultform;

          if not (resultform in [none, reals, doubles]) then
            warn(paramtypeerr);

          verifytoken(comma, nocommaerr);
          expression(follow + [rpar], false);

          if resultform = ints then
            begin
            newresulttype(realindex);
            genunary(float, ints)
            end;

          if not (resultform in [none, reals, doubles]) or
             (resultform <> firstform) then
            warn(paramtypeerr);

          computeresult(false);
          genbinary(dummyarg2op, resultform);
          paramform := resultform;
          parseextraargs;
          end
        else illegalident(varindex)
      end; {mc68881_realfn2}
      {>>>}
      {<<<}
      procedure mc68881_fint;
      { Process the special 68881 inline function FINT.  The argument is real or double, the result is integer }

      begin
        if sharedPtr^.switcheverplus[fpc68881] then
          begin
          beginparams;
          finishparams([none, reals, doubles], intindex);
          paramform := ints;
          end
        else
          illegalident (varindex)
      end;
      {>>>}
      {<<<}
      procedure mc68881_intfn(procid: standardids {func no.} );
      { Process a special 68881 inline function with a constant integer argument }

      begin
        if sharedPtr^.switcheverplus[fpc68881] then
          begin
          verifytoken (lpar, nolparerr);
          pushconstant (follow + [rpar]);
          if procid = fmovecrid then
            finishparams ([none, ints], realindex)
          else
            finishparams ([none, ints], intindex)
          end
        else
          illegalident (varindex)
      end;
      {>>>}

      begin {standardfunctions}
        if not (procid in
           [refid, loopholeid, sizeid, bitsizeid, ordid, chrid, succid, predid,
           lengthid, concatid, snglid, dblid, upperid, lowerid]) then
          genlit(ord(procid));
        gettoken;
        case procid of
          posid: pos;
          copyid: copy;
          concatid: concat;
          sinid, cosid: sincosfn;
          expid, lnid, sqrtid, arctanid: transcendentals;
          lengthid: lengthfunction;
          timeid: time;
          oddid: oddfunction;
          absid: absfunction;
          sqrid: sqrfunction;
          truncid, roundid: truncround;
          ordid: ordfunction;
          chrid: chrfunction;
          succid, predid: succpred;
          eofid, eolnid: eofeoln;
          sizeid, bitsizeid: sizefunction;
          lowerid, upperid: lowerupperfunction;
          loopholeid: loopholefunction;
          refid: reffunction;
          ioerrorid: iofunction(boolindex);
          iostatusid: iofunction(intindex);
          snglid: snglfn;
          dblid: dblfn;

          facosid, fasinid, fatanid, fatanhid, fcoshid, fetoxm1id, fgetexpid,
          fgetmanid, flog10id, flog2id, flognp1id, fsinhid, ftanid, ftanhid,
          ftentoxid, ftwotoxid:
            mc68881_realfn1(procid);

          fmodid, fremid, fscaleid, fsgldivid, fsglmulid:
            mc68881_realfn2(procid);

          fmovecrid, readfpcrid: mc68881_intfn(procid);

          fintid: mc68881_fint;

          otherwise warn(compilerwritererr)
          end;
        if not (procid in
           [refid, sizeid, bitsizeid, ordid, chrid, loopholeid, succid, predid,
           lengthid, concatid, snglid, dblid, upperid, lowerid]) then
          genunary(sysfn, paramform);
      end {standardfunctions} ;
    {>>>}
    {<<<}
    procedure setexpression;
    { Parse an expression which is part of a set constructor.  This checks
      that the expression is compatible with prior expressions in the
      set constructor, and that it is a legal base type for a set.

      This kludge is necessary because there is no way to tell the type of
      a constructed set except by guessing from the  expressions included.

      The basetype is set to "noneindex" before the first expression is
      parsed, and this is compatible with any type.
    }

      begin {setexpression}
        expression(follow + [dotdot, comma, rbrack], false);
        setisconst := setisconst and (oprndstk[sp].operandkind = constoperand);
        with setentry do
          begin
          if not compatible(basetype, resulttype) then
            warnbefore(badsetexpression)
          else
            begin
            stripsubrange(resulttype);
            basetype := resulttype;
            end;
          if basetype = intindex then basetype := subrangeindex;
          baseptr := ref(bigtable[basetype]);
          if not (getform(baseptr) in
             [ints, chars, bools, scalars, subranges, none]) then
            warnbefore(badsetbase)
          else gencheck(rangechkop, basetype);
          end;
      end {setexpression} ;
    {>>>}

  begin
    newresulttype (noneindex);

    if token in begfactset then
      case token of
        {<<<}
        stringconst:
          begin
          thistoken.pos := sharedPtr^.stringfilecount + 1;
          dumpstr (thistoken.len + 1, sharedPtr^.curstringbuf, true);
          pushconstant (follow);
          resultptr := ref(bigtable[resulttype]);
          resultptr^.align := 0;
          resultptr^.disposable := true;
          end;
        {>>>}
        {<<<}
        intconst, charconst, realconst, dblrealconst, nilsym:
          pushconstant(follow);
        {>>>}
        {<<<}
        ident:
          begin
          search(varindex);
          if varindex = 0 then illegalident(varindex)
          else
            begin
            varptr := ref(bigtable[varindex]);
            with varptr^ do
              case namekind of
                noname:
                  illegalident(varindex);
                constname, scalarname, typename:
                  {<<<}
                  begin
                  pushconstant(follow);
                  if token in [lbrack, dot, uparrow] then
                    begin
                    if token = lbrack then warnnonstandard(arrayexpected)
                    else if token = dot then warnnonstandard(recordexpected);
                    constpart := 0;
                    unpacking := false;
                    constantlen := sizeof(resultptr, false);
                    dumpconst(constantlen, false);
                    genoprnd;
                    sp := sp + 1;
                    oprndstk[sp].operandkind := varoperand;
                    oprndstk[sp].cost := 0;
                    ownvar := false;
                    selector(true, false, true, unpacking, constpart,
                             constantlen, off, varlev, ownvar);
                    lastindex(unpacking, constpart, constantlen);
                    if unsigned(resultptr, constantlen, unpacking) then
                      genop(unsvarop)
                    else
                      genop(varop);

                    with oprndstk[sp].value_range do
                      begin
                      settyperange(resultptr, optimistic);
                      pessimistic := optimistic;
                      end;

                    genint (constantlen);
                    genint (0);
                    genint (lastfilekey);
                    genint (0);
                    lastfilekey := lastfilekey - 1;
                    oprndstk[sp].oprndlen := sizeof(resultptr, false);
                    end;
                  end;
                  {>>>}
                varname, fieldname, param, varparam, confparam, varconfparam,
                boundid:
                  variable(true, true, true, false, true, varindex);
                forwardfunc, externalfunc, funcname: procedurecall(varindex);
                funcparam: paramcall(varindex);
                procname, forwardproc, externalproc, procparam, standardproc:
                  {<<<}
                  begin
                  warn(badprocfunc);
                  illegalident(varindex);
                  end;
                  {>>>}
                undefname, undeftypename:
                  illegalident(varindex);
                standardfunc:
                  standardfunctions(procid);
                end;
            end;
          end;
        {>>>}
        {<<<}
        notsym:
          begin
          gettoken;
          factor;
          if sharedPtr^.switchcounters[standard] > 0 then checkboolean
          else if not (resultform in [ints, bools, none]) then
            warnbefore(badarithtype);
          genunary(notop, resultform);
          end;
        {>>>}
        {<<<}
        lpar:
          begin
          gettoken;
          expression(follow + [rpar], false);
          verifytoken(rpar, norparerr);
          end;
        {>>>}
        {<<<}
        lbrack:
          begin
          gettoken;
          enterform (sets, settype, setptr);
          setentry := setptr^;
          setisconst := true;
          bumpsp;
          with oprndstk[sp] do
            begin
            oprndlen := 0;
            typeindex := settype;
            extended := false;
            operandkind := constoperand;
            cvalue.representation := sets;
            new(cvalue.setvalue);
            cvalue.setvalue^ := [];
            end;
          with setentry do
            begin
            size := (maxsetord + 1) div bitsperunit;
            basetype := noneindex;
            constructedset := true;
            genop(newset);
            while token in
                  [dotdot, comma, eql..andsym, ident, intconst..stringconst,
                  lpar, notsym, nilsym] do
              begin
              setexpression;
              if token = dotdot then
                begin
                setelementtype := resulttype;
                gettoken;
                setexpression;
                genbinary(setpair, ints);
                end
              else genunary(setelt, ints);
              if token <> rbrack then verifytoken(comma, nocommaerr);
              genoprnd;
              end;
            baseptr := ref(bigtable[basetype]);
            if getform(baseptr) in
               [ints, chars, bools, scalars, subranges] then
              begin
              size := ((upper(baseptr) + bitsperunit) div bitsperunit);
              if not packedflag and (size > unitsize) then
                size := forcealign(size, setalign, false);
              end;
            oprndstk[sp].oprndlen := size;
            end;
          setptr := ref(bigtable[settype]);
          setptr^ := setentry;
          newresulttype(settype);
          verifytoken(rbrack, norbrackerr);
          genunary(bldset, sets);
          {kludge: can't make it const as genoprnd will re-emit the structop,
           can't leave it exproperand as param push will allow modification.
           Could have made a special operandtype for this but seemed like
           too much trouble.
          }
          if setisconst then oprndstk[sp].operandkind := varoperand;
          end
        {>>>}
        end
    else
      begin
      warnbetween (nooprnderr);
      bumpsp;
      oprndstk[sp].operandkind := exproperand;
      oprndstk[sp].typeindex := noneindex
      end
  end;
  {>>>}
  {<<<}
    procedure expression {follow : tokenset; (legal following symbols)
                          arrayopt:boolean (true if array opt wanted)} ;
    {<<<}
    { Syntactic routine to parse an expression (with mods).

      Productions:

      expression = simple-expression [ relational-operator
            simple-expression ]  .

      relational-operator = "=" | "<>" | "<" | ">" | "<=" |
            ">=" | "in"  .

      The funny part about this routine is that an initial factor may have
      been parsed already when the routine is called.  This is indicated
      by setting "skipfactor" in the call.  This kludge is necessary because
      the first argument to a write procedure may be just a factor, and
      may be an entire expression. (File defaults are a pain.)

      The "linearize" parameter is set only when computing an array subscript,
      and informs the constant folder that we would like to incorporate any
      additive constants into the constant part of the array address.
    }
    {>>>}

      var
        lefttype: tableIndex; {type of the left operand}
        leftptr: entryptr; {pointer to left node, for the 'in' operator}
        op: tokentype; { relational operator (if any) }
        opseen: boolean; {set if there is actually an operator}

      {<<<}
      procedure term;
      {<<<}
      { Syntactic routine to parse a term.
        term = factor [* multiplying-operator factor *]  .

        multiplying-operator = "*" | "/" | "div" | "mod" | "and"  .

        If the global "skipfactor" is set in "expression", the first call to
        factor is assumed to have been made prior to the call to term.  This
        parameter is reset to avoid skipping more factors.

        On machines where doing a divide gives a free mod the "div" and "mod"
        operators are split into two parts in the intermediate
        file.  A binary operator "divop" performs the division, and unary operators
        "remop" or "quoop" pick either the remainder or the quotient.  This takes
        advantage of the "almost universal" machine characteristic of generating
        both pieces on a divide, and allows the results to be used as common
        subexpressions.
        On those machines not of this "universe" ( i.e. ns32k )
        "div" and "mod" are treated just like any other binary operator.
       }
      {>>>}

      var
        op: tokentype; {operator (if found)}
        specialdiv: boolean; {may need to correct remainder for mod}
        f: entryptr; {used to get lower bound for div/mod}

      begin
        if not skipfactor then
          factor;
        skipfactor := false;

        while token in termops do
          begin
          if (token = slash) and (resultform = ints) then
            begin
            newresulttype (realindex);
            genunary (float, ints)
            end;
          op := thistoken.token;

          gettoken;
          factor;
          computeresult (false);

          if (op = andsym) and (sharedPtr^.switchcounters[standard] > 0) then
            checkboolean
          else
            begin
            case op of
              {<<<}
              andsym:
                if not (resultform in [ints, bools, none]) then
                  warnbefore (badarithtype);
              {>>>}
              {<<<}
              star:
                if not (resultform in [sets, ints, reals, doubles, none]) then
                  warnbefore (badarithtype);
              {>>>}
              {<<<}
              divsym, modsym:
                begin
                specialdiv := true;

                with oprndstk[sp - 1] do
                  if (value_range.pessimistic.minlimit >= 0) or
                     extended then
                    with oprndstk[sp] do
                      if (value_range.pessimistic.minlimit >= 0) or
                         extended then
                        specialdiv := false
                      else
                        begin
                        f := ref(bigtable[oprndstk[sp - 1].typeindex]);
                        if (lower(f) >= 0) or oprndstk[sp - 1].extended then
                          begin
                          f := ref(bigtable[oprndstk[sp].typeindex]);
                          if (lower(f) >= 0) or oprndstk[sp].extended then
                            specialdiv := false;
                          end;
                        end;

                if specialdiv then
                  genbinary (stddivop, ints)
                else
                  genbinary (divop, ints);
                if not (resultform in [ints, none]) then
                  warnbefore (badarithtype);

                end;
              {>>>}
              {<<<}
              slash:
                if not (resultform in [reals, doubles, none]) then
                  warnbefore (badarithtype);
              {>>>}
              end;
            end;

          if op in [divsym, modsym] then
            genunary(optable[op], resultform)
          else
            genbinary (optable[op], resultform);
          end;
      end;
      {>>>}
      {<<<}
      procedure simpleexpression;
      { Syntactic routine to parse a simple-expression.
        Productions:
        simple-expression = [ sign ] term [* adding-operator term *]  .
        adding-operator = "+" | "-" | "or"  .
        No special action is taken.
      }

        var
          op: tokentype; {operator if found}
          signed: boolean; {set if initial sign found}
          negate: boolean; {set if initial sign was minus}


        begin {simpleexpression}
          signed := (token in [plus, minus]) and not skipfactor;
          if signed then
            begin
            negate := (token = minus);
            gettoken;
            end;
          term;
          if signed then
            begin
            if resultform = sets then warnbefore(signedseterr)
            else if not (resultform in [ints, reals, doubles, none]) then
              warnbefore(badarithtype);
            if negate then genunary(negop, resultform);
            end;
          while token in sexprops do
            begin
            op := thistoken.token;
            gettoken;
            term;
            computeresult(op = plus);
            if (op = orsym) then
              begin
              if ((sharedPtr^.switchcounters[standard] > 0) and (resultform = ints) or
                 not (resultform in [bools, ints, none])) then
                warnbefore(badarithtype)
              end
            else if not ((resultform in [ints, reals, doubles, sets, none]) or
                    (op = plus) and (resultform = strings)) then
              warnbefore(badarithtype);
            linearize := arrayopt;
            genbinary(optable[op], resultform);
            end;
        end {simpleexpression} ;
      {>>>}

      begin {expression}
        simpleexpression;
        while (token in begexprset) and ((token <> ident) or
              (thistoken.line = lasttoken.line)) do
          begin
          lefttype := resulttype;
          if token in exprops then
            begin
            op := thistoken.token;
            opseen := true;
            gettoken
            end
          else
            begin
            opseen := false;
            op := eofsym;
            warnbetween(nooperr)
            end;
          simpleexpression;
          if op = insym then
            begin
            leftptr := ref(bigtable[lefttype]);
            if (resultform <> sets) or
               not (leftptr^.typ in
               [none, ints, chars, scalars, bools, subranges]) or not compatible(lefttype,
                                                           resultptr^.basetype)
               then
              warnbefore(badinoprnds);
            end
          else
            begin
            computeresult(false);
            if resultform = arrays then
              begin
              if not resultptr^.stringtype then warnbefore(badreloprnds);
              end
            else
              begin
              case op of
                lss, gtr:
                  begin
                  if resultform = sets then warnbefore(nostrictinclusion)
                  else if not (resultform in
                          [sets, ints, reals, doubles, bools, chars, scalars,
                          strings, none]) then
                    warnbefore(badarithtype)
                  end;
                eql, neq:
                  if not (resultform in
                     [sets, ptrs, ints, reals, doubles, bools, chars, scalars,
                     strings, none]) then
                    warnbefore(badarithtype);
                leq, geq:
                  if not (resultform in
                     [sets, ints, reals, doubles, bools, chars, scalars, strings,
                     none]) then
                    warnbefore(badarithtype);
                otherwise {do nothing} ;
                end;
              end;
            end;
          if opseen then genbinary(optable[op], resultform)
          else if sp >= 0 then sp := sp - 1;
          newresulttype(boolindex);
          with oprndstk[sp], value_range do
            begin
            typeindex := resulttype;
            oprndlen := scalarsize;
            { Set new result range however if we already have the result due to folding don't throw it away! }
            if operandkind <> constoperand then
              begin
              settyperange (resultptr, optimistic);
              pessimistic := optimistic;
              end;
            end;
          verify1(follow + [rpar] + begexprset, badexprerr);
          end;

        while not (rpar in follow) and (token = rpar) do
          begin
          warn(badrparerr);
          gettoken
          end;

        verify1(follow, badexprerr);
      end {expression} ;
  {>>>}

  {<<<}
  procedure standardprocedures(procid: standardids {which std proc} );
  { Parse a standard procedure.  The syntax is similar to that for a user
    procedure, so will not be repeated here.

    This is complicated by the "default" file parameter for read and write,
    and the variety of types which these procedures accept.  Also, new and
    dispose require special processing for the optional tag arguments.
  }
  {<<<}
      procedure pushstringparam(extendedstring: boolean {arrays or strings} );

  { Push a string parameter onto the operand stack.  This actually generates
    code to push a reference to the string, followed by a the string length.

    Both standard Pascal strings (packed array [1..n] of char) and extended
    strings (string[n]) are allowed.

    This is used to handle string parameters to "write" and for the optional
    file specification parameters to reset and rewrite.
  }

        var
          stringlen: integer; {length of the string}


        begin {pushstringparam}
          if extendedstring then
            begin
            oprndstk[sp].oprndlen := sharedPtr^.ptrsize + defaulttargetintsize;
            genunary(pushstraddr, strings);
            end
          else
            begin
            stringlen := resultptr^.arraymembers;
            if constcheck(sp) then dumpconst(stringlen, false);
            oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
            genunary(pushaddr, ints);
            genpushdefaultint(stringlen);
            end;
        end {pushstringparam} ;
  {>>>}
  {<<<}
      procedure ioprocedure(textfileneeded: boolean {must be a text file} );

  { Parse simple, one argument file procedures.
  }


        begin {ioprocedure}
          fileparam(true, false, textfileneeded);
          parseextraargs;
        end {ioprocedure} ;
  {>>>}
  {<<<}
      procedure seek;

  { Process a "seek" procedure. First parameter is file, but otherwise
    nothing special.
  }


        begin {seek}
          fileparam(true, false, false);
          verifytoken(comma, nocommaerr);
          expression(follow + [comma, rpar], false);
          if not (resultform in [none, ints]) then warn(badfunctionarg);
          setdefaulttargetintsize;
          genunary(pushvalue, ints);
          parseextraargs;
        end {seek} ;
  {>>>}
  {<<<}
      procedure getname(blankallowed: boolean {true sez blank field ok} );

  { Get a file name parameter for reset or rewrite.  This must be a string of
    some flavor, or blank (i.e. reset(f,'x',,i) ).
  }


        begin {getname}
          verifytoken(comma, nocommaerr);
          if blankallowed and (token = comma) then
            begin
            sp := sp + 1;
            oprndstk[sp] := nilvalue;
            genunary(pushvalue, ptrs);
            genpushdefaultint(0);
            end
          else
            begin
            expression(follow + [comma, rpar, intconst..stringconst], false);
            if (resultform = arrays) and resultptr^.stringtype or
               (resultform = strings) then
              pushstringparam(resultform = strings)
            else if (resultform <> none) then warnbefore(nostringerr);
            end;
        end {getname} ;
  {>>>}
  {<<<}
  procedure rename;
  { Process the "rename" procedure.  Both the file and string parameters are required. }

  begin
    fileparam (true, false, false);
    getname (false);
    parseextraargs;
  end;
  {>>>}
  {<<<}
  procedure resetrewrite;
  { Process a "reset" or "rewrite" procedure.  This has to deal with the optional parameters for file specifications. }

  begin
    fileparam(true, true, false);
    genunary(setfileop, none);
    if token in
       [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar,
       notsym, nilsym] then
      begin
      warnnonstandard(filenameerr);
      getname(true);
      if token in
         [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar,
         notsym, nilsym] then
        getname(true);
      if token in [comma, ident] then
        begin
        verifytoken(comma, nocommaerr);
        modifyvariable(true, false);
        if not identical(resulttype, intindex) then
          warnbefore(paramtypeerr);
        oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
        genunary(pushaddr, ints);
        end;
      end;
    parseextraargs;
  end;
  {>>>}
  {<<<}
  procedure newdispose;
  { Parse a "new" or "dispose" call.  The optional arguments are used to chain
    down the tree of variants to get the size of the specified variant.  The
    final size result is passed to the routine as a second argument.
  }
  var
    n: tableIndex; {name entry, from search}
    p: entryptr; {used to get currentrecord from name table}
    currentrecord: tableIndex; {current part of record spec}
    currentptr: entryptr; {used to access currentrecord}
    f: entryptr; {used for general form access}
    callnew: boolean; {true if new instead of dispose}
    containedfile: boolean; {true if parent record contains file}

  begin
    containedfile := false;

    callnew := procid = newid;
    genop(bldnil);

    verifytoken(lpar, nolparerr);
    newresulttype(noneindex);

    if token = ident then
      begin
      if callnew then
        modifyvariable(true, true)
      else
        begin
        search(n);
        if n <> 0 then
          begin
          p := ref(bigtable[n]);
          if p^.namekind in
             [funcname, forwardfunc, externalfunc, funcparam] then
            factor
          else
            modifyvariable(true, true);
          end
        else
          illegalident(n);
        end;
      if not (resultform in [none, ptrs]) then
        warnbefore(noptrvar);
      genunary(pushaddr, ptrs);
      end
    else
      warnbetween(novarerr);

    currentrecord := noneindex;
    with resultptr^ do
      if typ = ptrs then
        begin
        p := ref(bigtable[ptrtypename]);
        currentrecord := p^.typeindex;
        if p^.namekind = undeftypename then
          begin
          p^.lastoccurrence := display[level].scopeid;
          end;
        currentptr := ref(bigtable[currentrecord]);
        containedfile := currentptr^.containsfile;
        end;

    parsetagparams(currentrecord);
    currentptr := ref(bigtable[currentrecord]);
    pushint(sizeof(currentptr, false));
    oprndstk[sp].oprndlen := defaultptrsize;
    genunary(pushvalue, ints);
    parseextraargs;

    if not callnew and containedfile then
      genunary(closerangeop, none);
  end;
  {>>>}
  {<<<}
  procedure filehack(filetype: tableIndex; reading: boolean);
  {<<<}
  { Generate intcode to access one file element.  The vast number of indirect
    operations is due to the fact that we placed the address of the file var
    on the stack when it was processed for the forthcoming "get/put" operation.
    Thus, we have mem[sp]->filevar->buffptr->data, three indirects in all.
  }
  {>>>}

  var
    f: entryptr; {for access to filetype}

  begin
    genunary (setbinfileop, files);
    genunary (indrop, files);

    if reading then
      genunary (definelazyop, files);

    genunary (indrop, files);
    genunary (indrop, files);
    genop (newvarop);
    bumpsp;

    f := ref(bigtable[filetype]);
    newresulttype (f^.filebasetype);

    with oprndstk[sp] do
      begin
      typeindex := resulttype;
      oprndlen := sizeof(resultptr, false);
      genint (oprndlen);
      operandkind := exproperand;
      cost := 0;
      extended := resultptr^.extendedrange;
      end;

    genint (0);
    genint (0);
    genint (0);
  end;
  {>>>}
  {<<<}
  procedure gencopystack(reservelen: integer);
  { Generate special code to copy the top element of the runtime stack,
    which is assumed by this routine to be the explicitly named file
    argument to "read" or "write".  Complicated by the fact that the
    travrs operand stack (for building nodes) might or might not already
    contain the read/write argument.
  }
  begin
    if not constcheck(sp) then
      genop (switchstack);
    genop (copystackop);
    genint (reservelen);
    genint (0);
    genform (ptrs);
    if not constcheck(sp) then
      genop(switchstack);
  end;
  {>>>}
  {<<<}
  procedure readprocedure;
  { Process a read procedure call.  The first argument may be a file, in
    which case that file is used, otherwise the file "input" is used.
  }
  var
    filetype: tableIndex;
    f: entryptr; {for access to filetype}
    readflag: boolean; {true if "read", rather than "readln"}

    {<<<}
          procedure readparams;

    { Parse read parameters.  These look like any other var parameters,
      except that many types of parameters are allowed.
    }

            var
              readform: types; {type of parameter}
              readfile: boolean; {true if read(filevar, ...) form}
              restoresp: - 1..oprnddepth; {for restoring operand sp after each read}
              filetype: tableIndex;
              readfiledeclared: boolean; {set true if file was declared in program
                                          header}

    {<<<}
            procedure genoneread;

    { Generate the intermediate file output for one read parameter.  This
      is done with a special operation "rd".
    }

              var
                resultlen: integer; {length of result from read}


              begin {genoneread}
                if readform <> files then
                  begin
                  if readform in [ints, reals, doubles, chars] then
                    begin
                    if readform = ints then resultlen := defaulttargetintsize
                    else resultlen := sizeof(resultptr, false);
                    gencopystack(resultlen);
                    end
                  else if readfile and ((token <> rpar) or not readflag) then
                    gencopystack(0);

                  if not (readform in [none, ints, reals, doubles, chars]) then
                    begin
                    if ((readform <> arrays) or not resultptr^.stringtype) and
                       (readform <> strings) or (sharedPtr^.switchcounters[standard] > 0) then
                      warnbefore(badreadtype)
                    else pushstringparam(false);
                    end;
                  end;
                genunary(rd, readform);
              end {genoneread} ;
    {>>>}
    {<<<}
            procedure onereadparam;

    { Parse one read parameter.  This must be a variable.
    }


              begin {onereadparam}
                sp := restoresp;
                if token = ident then
                  begin
                  if filetype <> textindex then filehack(filetype, true);
                  modifyvariable(true, true);
                  readform := resultform;
                  if filetype <> textindex then
                    begin
                    f := ref(bigtable[filetype]);
                    if not compatible(resulttype, f^.filebasetype) then
                      warnbefore(typesincomp);
                    genop(switchstack);
                    genbinary(moveop, readform);
                    readform := files;
                    end;
                  parsecolons(0);
                  verify1([rpar, comma, ident], badparamerr)
                  end
                else
                  begin
                  warnbetween(novarerr);
                  newresulttype(noneindex);
                  end;
              end {onereadparam} ;
    {>>>}


            begin {readparams}
              sp := - 1;
              readfile := false;
              restoresp := sp;
              verifytoken(lpar, nolparerr);
              filetype := textindex;
              readfiledeclared := filedeclared;
              onereadparam;
              if resultform = files then
                begin
                if not readfiledeclared then warnbefore(filenotdeclared);
                restoresp := sp;
                filetype := resulttype;
                if (filetype <> textindex) and (procid = readlnid) then
                  warnbefore(nottextfile);
                if (procid = readid) and (token = rpar) then warn(noreadarg);
                genunary(pushaddr, ptrs);
                if filetype = textindex then genunary(setfileop, none);
                readfile := true;
                end
              else
                begin
                if not inputdeclared then
                  warnnonstandard (inputnotdeclared);
                genoneread;
                end;
              while token in [comma, ident] do
                begin
                verifytoken (comma, nocommaerr);
                onereadparam;
                if resultform = files then
                  warnbefore(badreadtype);
                genoneread;
                end;

              parseextraargs;
            end;
    {>>>}

  begin
    genop(bldnil);
    readflag := procid = readid;
    if readflag and (token <> lpar) then
      warnbetween(noreadarg)
    else if token = lpar then
      readparams
    else
      begin
      if not inputdeclared then
        warnnonstandard(inputnotdeclared);
      end;
  end;
  {>>>}
  {<<<}
  procedure writeprocedure;
  { Process a write procedure.  This is complicated by the optional file
    argument (as in read) and by the optional field width arguments set
    by colons.  As in the read procedure, each write argument is passed
    with a special argument.
  }
  var
    writeflag: boolean; {true if write instead of writeln}
    filetype: tableIndex;
    f: entryptr; {for access to filetype}

    {<<<}
    procedure writeparams;
    { Parse the parameters to a write procedure. }

    var
      restoresp: - 1..oprnddepth; {used to restore operand stack}
      writefiledeclared: boolean; {set true if file was declared in program header}

      {<<<}
      procedure onewriteparam (varread: boolean);
      {<<<}
      { Parse a single parameter to a write procedure.  The flag "varread" is
        set if a variable has been read to check for the default file parameter.
      }
      {>>>}

      var
        writeform: types; {form of argument}
        writelen: addressrange; {length of write parameter}
        stringflag: boolean; {used to check for string argument}
        basetype: tableIndex; {pointer to base type}

      begin
        if filetype <> textindex then
          filehack(filetype, false);

        skipfactor := varread;
        expression(follow + [comma, rpar, colon], false);
        stripsubrange(resulttype);
        newresulttype(resulttype);
        writeform := resultform;

        if writeform = ints then
          setdefaulttargetintsize;
        writelen := oprndstk[sp].oprndlen;

        if filetype = textindex then
          begin
          {set stringflag before upcoming genunary modifies resulttype}
          if writeform = arrays then
            stringflag := resultptr^.stringtype
          else
            stringflag := writeform = strings;

          if ((token <> rpar) or not writeflag) then
            gencopystack(0);

          case writeform of
            bools, chars, ints:
              begin
              genunary(pushvalue, writeform);
              parsecolons(1);
              end;

            none, reals, doubles:
              begin
              genunary(pushvalue, writeform);
              parsecolons(2);
              end;

            arrays, strings:
              begin
              if not stringflag then warnbefore(badwritearg)
              else pushstringparam(writeform = strings);
              parsecolons(1)
              end;

            otherwise
              begin
              warnbefore(badwritearg);
              parsecolons(maxint)
              end
            end;
          end
        else
          begin
          f := ref(bigtable[filetype]);
          basetype := f^.filebasetype;
          f := ref(bigtable[basetype]);
          if ((getform(f) = ints) and ((writeform = reals) or (writeform = doubles))) or
              not compatible(basetype, resulttype) then
              warnbefore(typesincomp);
            genbinary (moveop, resultform);
            writeform := files;
          end;

        genop (wr);
        genint (writelen);
        genint (0);
        genform (writeform);
        sp := restoresp;
      end;
      {>>>}

    begin
      restoresp := - 1;
      verifytoken (lpar, nolparerr);

      filetype := textindex;
      if token = ident then
        begin
        writefiledeclared := filedeclared;
        factor;
        if resultform = files then
          begin
          if not writefiledeclared then
            warnbefore (filenotdeclared);
          filetype := resulttype;

          verify1([comma, rpar], badparamerr);
          genunary (pushaddr, ptrs);
          restoresp := sp;
          if filetype = textindex then
            genunary (setfileop, none);
          if (filetype <> textindex) and (procid = writelnid) then
            warnbefore (nottextfile);
          if writeflag and (token = rpar) then
            warn (nowritearg)
          end
        else
          begin
          if not outputdeclared then
            warnnonstandard (outputnotdeclared);
          onewriteparam (true);
          end
        end
      else
        begin
        if not outputdeclared then
          warnnonstandard (outputnotdeclared);
        onewriteparam (false);
        end;

      while token in [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar, notsym, nilsym] do
        begin
        verifytoken (comma, nocommaerr);
        onewriteparam (false)
        end;

      parseextraargs;
    end;
    {>>>}

  begin
    genop (bldnil);

    writeflag := (procid = writeid);
    if not writeflag and (token <> lpar) then
      begin
      if not outputdeclared then
        warnnonstandard(outputnotdeclared);
      end;

    if writeflag then
      verify([lpar], begexprset, nolparerr);

    if token in begexprset then
      writeparams;
  end;
  {>>>}
  {<<<}
  procedure pusharrayparam (packedarray, {which kind of array}
                            assigning: boolean; {true if assigning new value}
                            var indxtype: tableIndex; {type of index}
                            var elttype: tableIndex {type of element} );
  { Push one array parameter for the intrinsic routines pack/unpack.
    Items pushed include the address of the array, lower/upper bounds,
    and size.  If packedarray is true, ord(bitaddress) is pushed as well.
  }
  var
    varindex: tableIndex; {used to search variable}
    eltsize: addressrange; {size of one array element}
    packedelt: boolean; {set true if element is < bitsperunit}
    packing: boolean; {set if a packed array}
    highid, lowid: tableIndex; {id's for bound identifiers}
    f: entryptr; {used to get index and element data}

  begin
    if token = ident then
      search(varindex);
    if assigning or (token <> ident) or (varindex = 0) then
      modifyvariable (true, true)
    else
      variable (true, true, false, true, true, varindex);

    f := nil;
    if resultform in [arrays, conformantarrays] then
      begin
      with resultptr^ do
        begin
        if packedflag <> packedarray then
          warnbefore (badparamerr);
        elttype := elementtype;
        indxtype := indextype;
        eltsize := elementsize;
        packedelt := packedflag;
        packing := packedarray;
        if resultform = conformantarrays then
          begin
          highid := highbound;
          lowid := lowbound;
          end
        else
          f := ref(bigtable[indextype]);
        end;

      oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
      genunary (pushaddr, ints);
      if resultform = conformantarrays then
        begin
        variable (true, false, false, false, false, lowid);
        setdefaulttargetintsize;
        genunary (pushvalue, ints);
        variable (true, false, false, false, false, highid);
        setdefaulttargetintsize;
        genunary (pushvalue, ints);
        end
      else
        begin
        genpushdefaultint (lower(f));
        genpushdefaultint (upper(f));
        end;
      if packedelt and (eltsize > bitsperunit) then
        begin
        packedelt := false;
        eltsize := eltsize div bitsperunit;
        end;
      genpushdefaultint (eltsize);

      if packing then
        begin
        genpushbool (packedelt);
        f := ref(bigtable[elttype]);
        genpushbool (unsigned(f, eltsize, packedelt));
        end;
      end
    else
      begin
      warnbefore (arrayexpected);
      elttype := noneindex;
      end;
  end;
  {>>>}
  {<<<}
      procedure pack;

  { Compile code for the pack procedure.  Since nobody in his right
    mind would use this silly thing except for compatibility with
    existing, archaic implementations, no effort has been made to
    make this procedure particularly efficient.  Indeed, a general
    packing routine is called at runtime, and is guaranteed to be
    less efficient than the equivalent code expressed in source form
    as a for-loop.
  }

        var
          indextype, lefttype, righttype: tableIndex; {types of left and right arrays}


        begin {pack}
          indextype := 0; { Causes problems if not initialized and type is "none"}
          lefttype := 0;
          righttype := 0;
          genop(bldnil);
          verifytoken(lpar, nolparerr);
          pusharrayparam(false, false, indextype, lefttype);
          verifytoken(comma, nocommaerr);
          expression(follow + [comma, rpar], false);
          if not compatible(indextype, resulttype) then warnbefore(badparamerr);
          setdefaulttargetintsize;
          genunary(pushvalue, ints);
          verifytoken(comma, nocommaerr);
          pusharrayparam(true, true, indextype, righttype);
          if not identical(lefttype, righttype) then warnbefore(badparamerr);
          verifytoken(rpar, norparerr);
        end {pack} ;
  {>>>}
  {<<<}
      procedure unpack;

  { Compile code for intrinsic procedure unpack.  Calls runtime for help.
  }

        var
          indextype, lefttype, righttype: tableIndex; {types of left and right arrays}


        begin {unpack}
          indextype := 0; { Causes problems if not initialized and type is "none"}
          lefttype := 0;
          righttype := 0;
          genop(bldnil);
          verifytoken(lpar, nolparerr);
          pusharrayparam(true, false, indextype, lefttype);
          verifytoken(comma, nocommaerr);
          pusharrayparam(false, true, indextype, righttype);
          if not identical(lefttype, righttype) then warnbefore(badparamerr);
          verifytoken(comma, nocommaerr);
          expression(follow + [comma, rpar], false);
          if not compatible(indextype, resulttype) then warnbefore(badparamerr);
          setdefaulttargetintsize;
          genunary(pushvalue, ints);
          verifytoken(rpar, norparerr);
        end {unpack} ;
  {>>>}
  {<<<}
      procedure page;

  { Compile standard procedure "page(filename)".  It has a non-standard
    form in that "output" is default.
  }


        begin {page}
          if token = lpar then
            begin
            ioprocedure(true);
            genunary(setfileop, none);
            end
          else
            begin
            if not outputdeclared then warnnonstandard(outputnotdeclared);
            end;
        end {page} ;
  {>>>}
  {<<<}
      procedure insert;

  { Parse standard procedure "insert(stringexpr, stringvar, pos)".
  }


        begin {insert}
          genop(bldnil);
          verifytoken(lpar, nolparerr);
          stringsource;
          verifytoken(comma, nocommaerr);
          stringtarget;
          verifytoken(comma, nocommaerr);
          expression(follow + [rpar], false);
          if not (resultform in [none, ints]) then warn(badfunctionarg);
          setshorttargetintsize;
          genunary(pushvalue, ints);
          parseextraargs;
        end {insert} ;
  {>>>}
  {<<<}
      procedure deletestr;

  { Parse "deletestr(st, pos, num)"
  }


        begin {deletestr}
          genop(bldnil);
          verifytoken(lpar, nolparerr);
          stringtarget;
          verifytoken(comma, nocommaerr);
          expression(follow + [comma], false);
          if not (resultform in [none, ints]) then warn(badfunctionarg);
          setdefaulttargetintsize;
          genunary(pushvalue, ints);
          verifytoken(comma, nocommaerr);
          expression(follow + [rpar], false);
          if not (resultform in [none, ints]) then warn(badfunctionarg);
          setdefaulttargetintsize;
          genunary(pushvalue, ints);
          parseextraargs;
        end {deletestr} ;
  {>>>}
  {<<<}
      procedure val;

  { Parse "val(str, value, errorpointer)".
  }


        begin {val}
          genop(bldnil);
          verifytoken(lpar, nolparerr);
          stringsource;
          verifytoken(comma, nocommaerr);
          modifyvariable(true, true);
          if not (identical(resulttype, intindex) or identical(resulttype,
             realindex) or identical(resulttype, doubleindex)) then
            warn(paramtypeerr);
          oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
          genunary(pushaddr, resultform);
          verifytoken(comma, nocommaerr);
          modifyvariable(true, true);
          if not identical(resulttype, intindex) then warn(paramtypeerr);
          oprndstk[sp].oprndlen := sharedPtr^.ptrsize;
          genunary(pushaddr, ints);
          parseextraargs;
        end {val} ;
  {>>>}
  {<<<}
      procedure str;

  { Parse "str(value, stringtarget)".
  }


        begin {str}
          genop(bldnil);
          verifytoken(lpar, nolparerr);
          expression(follow + [comma, colon], false);
          if not (resultform in [none, reals, doubles, ints]) then
            warn(badfunctionarg);
          if resultform = ints then setdefaulttargetintsize;
          genunary(pushvalue, resultform);
          if resultform = ints then parsecolons(1)
          else parsecolons(2);
          verifytoken(comma, nocommaerr);
          stringtarget;
          parseextraargs;
        end {str} ;
  {>>>}
  {<<<}
  procedure setfpcrproc;

  begin
    if sharedPtr^.switcheverplus[fpc68881] then
      begin
      genop (bldnil);
      verifytoken (lpar, nolparerr);
      pushconstant ([comma, rpar, semicolon]);

      if resultform <> ints then
        begin
        warnbefore (badfunctionarg);
        newresulttype (noneindex);
        end;

      genunary (dummyargop, resultform);
      verifytoken (comma, nocommaerr);
      expression (follow + [rpar], false);

      if not (resultform in [none, ints]) then
        warn (paramtypeerr);

      genunary (dummyargop, resultform);
      parseextraargs;
      end
    else
      illegalident(varindex)
  end;
  {>>>}
  {<<<}
  procedure fsincosproc;
  { Parse an fsincos procedure.  There are three arguments:  The first is
    the input argument, the second is the cosine and the third is the sine.
    The second and third arguments are output parameters.
  }

  var
    firstform: types; {form of first argument}

  begin
    if sharedPtr^.switcheverplus[fpc68881] then
      begin
      genop (bldnil);
      verifytoken (lpar, nolparerr);
      expression (follow + [comma], false);

      if resultform = ints then
        begin
        newresulttype (realindex);
        genunary (float, ints)
        end;

      genunary (dummyargop, resultform);
      firstform := resultform;

      if not (resultform in [none, reals, doubles]) then
        warn (paramtypeerr);

      verifytoken (comma, nocommaerr);
      modifyvariable (false, false);

      if not (resultform in [none, reals, doubles]) or
         (resultform <> firstform) then
        warn (paramtypeerr);

      genunary (dummyargop, resultform);

      verifytoken (comma, nocommaerr);
      modifyvariable (false, false);

      if not (resultform in [none, reals, doubles]) or
         (resultform <> firstform) then
        warn (paramtypeerr);

      genunary (dummyargop, resultform);
      parseextraargs;
      end
    else
      illegalident (varindex)
  end;
  {>>>}

  begin
    newexprstmt (syscall);
    genint (ord(procid));

    gettoken;
    case procid of
      readid, readlnid: readprocedure;
      writeid, writelnid: writeprocedure;
      newid: newdispose;
      disposeid: newdispose;
      pageid: page;
      putid, getid, breakid, closeid, deleteid, noioerrorid:
        ioprocedure(false);
      seekid: seek;
      renameid: rename;
      resetid, rewriteid: resetrewrite;
      packid: pack;
      unpackid: unpack;
      deletestrid: deletestr;
      insertid: insert;
      strid: str;
      valprocid: val;
      fsincosid: fsincosproc;
      setfpcrid: setfpcrproc;
      otherwise warn(compilerwritererr)
      end;
  end;
  {>>>}

  {<<<}
  procedure assignlabel;
  { Assign a label to the current statement.  This must search the labellist
    at the current level to make sure that it is declared.  It also may have
    to check and see if the label has been the target of an illegal goto.
  }
  var
    t: labelptr; {Label entry}

  begin {assignlabel}
    checkundefs := false;
    nolabelsofar := false;
    searchlabels(thistoken.intvalue, t);
    if (t = labelflag) or (lev <> level) then warn(labnotpredef)
    else
      with t^ do
        begin
        if definednest <> 0 then warn(badlabeldef)
        else if nest > maxlegalnest then
          warnat(badlabelnest, labelline, labelcolumn);
        definednest := nest;
        maxlegalnest := maxint;
        if nonlocalref then anynonlocallabels := true;
        end;
    genstmt(deflab);
    genint(t^.internalvalue);
    genint(lev);
    genint(ord(t^.nonlocalref));
    gettoken;
    if token = becomes then illegalassign
    else verifytoken(colon, nocolonerr);
  end {assignlabel} ;
  {>>>}
  {<<<}
  procedure badelseclause;
  { Generate an error message for a bad else clause, then recover.
    This is such a common error it is worth special handling.
  }
  begin {badelseclause}
    warn(badelseerr);
    gettoken;
    statement(follow)
  end {badelseclause} ;
  {>>>}
  {<<<}
  procedure assignment;
  { Syntactic routine to parse an assignment statement.
    assignment-statement = variable ":=" expression  .
    This routine must transform integer expressions to real if the
    target is real, and may issue a range check for a subrange
    assignment.
    The left hand side variable is marked as modified.
  }
  var
    varptr: entryptr; {Provides access to LHS variable}
    lefttype: tableIndex; {LHS type}
    leftform: types; {for compatibility checking}
    leftstdstring: boolean; {for char-to-std-string conversion}

  begin
    variable (true, true, false, true, true, varindex);
    lefttype := resulttype;
    leftform := resultform;
    leftstdstring := (resultform = arrays) and resultptr^.stringtype;

    if token = eql then
      begin
      warn(nobecomeserr);
      gettoken
      end
    else
      verifytoken (becomes, nobecomeserr);

    expression (follow, false);

    if resultptr^.containsfile then warnbefore(dontassignfile)
    else if (leftform = reals) and (resultform = ints) then
      genunary(float, ints)
    else if (leftform = ints) and (resultform = reals) then
      warnbefore(badrealtoint)
    else if (leftform = doubles) and (resultform = ints) then
      genunary(float_double, ints)
    else if (leftform = ints) and (resultform = doubles) then
      warnbefore(badrealtoint)
    else if (leftform = doubles) and (resultform = reals) and
            not sharedPtr^.switcheverplus[doublereals] then
      begin
      oprndstk[sp].oprndlen := doublesize;
      genunary(real_to_dbl, reals);
      end
    else if (leftform = reals) and (resultform = doubles) then
      warnbefore(baddbltoreal)
    else if (leftform = strings) and (resultform = chars) then
      genunary(chrstrop, strings)
    else if (leftform = strings) and (resultform = arrays) and
            resultptr^.stringtype then
      genunary(arraystrop, strings)
    else
      begin
      if (sharedPtr^.switchcounters[standard] <= 0) and leftstdstring and
         (resultform = chars) then
        begin
        newstringtype(resulttype, arrays, 1);
        newresulttype(resulttype);
        oprndstk[sp].typeindex := resulttype;
        end;
      if not compatible(lefttype, resulttype) then warnbefore(badassignment);
      end;

    gencheck(rangechkop, lefttype);
    if leftform = conformantarrays then
      begin
      genvalsize(lefttype, 1);
      genoprnd;
      genbinary(cmoveop, arrays);
      end
    else genbinary(moveop, leftform);
    genoprndstmt;
    varptr := ref(bigtable[varindex]);
    with varptr^ do
      if namekind in
         [varname, fieldname, param, varparam, confparam, varconfparam] then
        begin
        modified := true;
        parammodified := true;
        if (nest = 1) and nolabelsofar then knownvalid := true;
        end;
  end {assignment} ;
  {>>>}
  {<<<}
  procedure compoundstatement;
  { Syntactic routine to parse a compound statement.
    compound-statement = "begin" statement [* ";" statement *] "end"  .
    This does very little, just parses a statement sequence
  }
  begin {compoundstatement}
    gettoken;
    nest := nest + 1;
    statement(follow + [semicolon, endsym, otherwisesym] - [elsesym]);
    while not (token in [labelsym..functionsym, endsym, eofsym]) do
      begin
      if token = semicolon then gettoken
      else verify1([semicolon], nosemierr);
      statement([semicolon, endsym, otherwisesym] + follow - [elsesym])
      end;
    updatelabelnest;
    nest := nest - 1;
    verifytoken(endsym, noenderr);
  end {compoundstatement} ;
  {>>>}
  {<<<}
  procedure ifstatement;
  { Syntactic routine to parse an if statement.
    if-statement = "if" expression "then" statement
          [ "else" statement ]  .
    This must make a modification in the normal checking for undefined
    variable references.  This is because in a loop, it is possible for the
    variables to be initialized by another branch of the if on an earlier
    pass through the loop.  Thus if we are in a looping construct the
    checking is disabled
  }
  var
    oldcheck: boolean; {old value of checkundefs}

  begin {ifstatement}
    getexprstmt(begif);
    expression(follow + [elsesym, thensym, dosym], false);
    checkboolean;
    genoprndstmt;
    oldcheck := checkundefs;
    checkundefs := (loopfactor = 0);
    verifytoken(thensym, nothenerr);
    nest := nest + 1;
    statement(follow + [elsesym]);
    genstmt(endthen);
    if token = elsesym then
      begin
      updatelabelnest;
      gettoken;
      genstmt(begelse);
      statement(follow);
      genstmt(endelse);
      end;
    updatelabelnest;
    nest := nest - 1;
    checkundefs := oldcheck;
  end {ifstatement} ;
  {>>>}
  {<<<}
  procedure casestatement;
  { Syntactic routine to parse a case statement.
    case-statement = "case" expression "of"
          case-list-element [* ";" case-list-element *] [ ";" ]
          [ ( "otherwise" | "else" ) statement [ ";" ] ] "end"  .
    A list of used case labels is kept to allow checking for duplicate
    labels.
    A problem similar to that for "if" exists for undefined variable checks.
  }
  type
    caselabptr = ^caselabentry; {used to keep track of case labels}
    caselabentry =
      record
        next: caselabptr;
        value1: integer
      end;

  var
    oldcheck: boolean; {old value of checkundef}
    latestlabel: caselabptr; {start of case label list}
    p1: caselabptr; {used when deleting case label list}
    casetype: tableIndex; {type of case expression}

    {<<<}
    procedure caselabel;
    {<<<}
    { Read a single case label, check it against previously read case labels,
      then add it to the list of labels read.

      Output generated:

      case-label = "caselab(value1)"
    }
    {>>>}

      var
        p: caselabptr; {used to trace label list}
        p1: caselabptr; {used to generate new label entry}
        lab: operand; {constant label value}


      begin {caselabel}
        new(p1);
        with p1^ do
          begin
          next := latestlabel;
          constant(follow + begconstset + [comma, colon, elsesym, otherwisesym],
                   true, lab);
          value1 := lab.cvalue.intvalue;
          if not compatible(casetype, lab.typeindex) then
            warnbefore(badcaselabeltype);
          p := latestlabel;
          while (p <> nil) do
            begin
            if p^.value1 = lab.cvalue.intvalue then warnbefore(dupcaselabel);
            p := p^.next;
            end;
          genstmt(caselab);
          genint(lab.cvalue.intvalue);
          end;
        latestlabel := p1;
      end {caselabel} ;
    {>>>}
    {<<<}
    procedure onecase;
    {<<<}
    { Syntactic routine to parse a case-element.
      case-list-element = constant [* "," constant *] ":"
            statement  .
    }
    {>>>}


      begin {onecase}
        if token in begconstset then
          begin
          caselabel;
          while token in
                [comma, ident, plus, minus, nilsym, intconst..stringconst] do
            begin
            verifytoken(comma, nocommaerr);
            if token in begconstset then caselabel
            else warn(caselabelerr)
            end;
          verifytoken(colon, nocolonerr);
          statement(follow + [semicolon, intconst, realconst, dblrealconst,
                    charconst, ident, endsym, elsesym, otherwisesym]);
          updatelabelnest;
          genstmt(endcaseelt);
          end;
      end {onecase} ;
    {>>>}

  begin {casestatement}
    getexprstmt(begcase);
    expression(follow + [ofsym], false);
    genoprndstmt;
    if not (resultform in [ints, chars, bools, scalars, subranges, none]) then
      warnbefore(badcasetype);
    verifytoken(ofsym, nooferr);
    casetype := resulttype;
    latestlabel := nil;
    oldcheck := checkundefs;
    checkundefs := (loopfactor = 0);
    nest := nest + 1;
    onecase;
    while token in
          [semicolon, ident, nilsym, plus, minus, intconst..stringconst] do
      begin
      verifytoken(semicolon, nosemierr);
      onecase
      end;
    if token in [elsesym, otherwisesym] then
      begin
      warnnonstandard(caseelseerr);
      gettoken;
      if token = colon then
        begin
        warn(caselabelerr);
        gettoken;
        end;
      genstmt(casedef);
      statement(follow + [semicolon, endsym]);
      updatelabelnest;
      if token = semicolon then gettoken
      end;
    nest := nest - 1;
    while latestlabel <> nil do
      begin
      p1 := latestlabel^.next;
      dispose(latestlabel);
      latestlabel := p1;
      end;
    verifytoken(endsym, noenderr);
    genstmt(endcase);
    checkundefs := oldcheck;
  end {casestatement} ;
  {>>>}
  {<<<}
  procedure whilestatement;
  { Syntactic routine to parse a while statement.
    while-statement = "while" expression "do" statement  .
    loopfactor is used to determine if checking for undefinded
    vars should happen.
  }
  var
    oldcheck: boolean; {old value of checkundefs}

  begin {whilestatement}
    loopfactor := loopfactor + 1;
    getexprstmt(begwhile);
    expression(follow + [dosym], false);
    checkboolean;
    genoprndstmt;
    verifytoken(dosym, nodoerr);
    nest := nest + 1;
    oldcheck := checkundefs;
    checkundefs := false;
    statement(follow);
    checkundefs := oldcheck;
    updatelabelnest;
    nest := nest - 1;
    genstmt(endwhile);
    loopfactor := loopfactor - 1;
  end {whilestatement} ;
  {>>>}
  {<<<}
  procedure repeatstatement;
  { Syntactic routine to parse a repeat statement.
    repeat-statement = "repeat" statement [* ";" statement *]
          "until" expression  .
    Loopfactor is manipulated in a manner similar to while.
  }

  begin {repeatstatement}
    loopfactor := loopfactor + 1;
    gettoken;
    debugstmt(begrpt, lasttoken.line, lasttoken.filepos, lasttoken.fileIndex);
    nest := nest + 1;
    statement(follow + [untilsym, semicolon]);
    while token in [semicolon, beginsym..gotosym, ident] do
      begin
      verifytoken(semicolon, nosemierr);
      statement(follow + [untilsym, semicolon])
      end;
    verifytoken(untilsym, nountilerr);
    genstmt(endrpt);
    intstate := opstate;
    expression(follow, false);
    checkboolean;
    genoprndstmt;
    updatelabelnest;
    nest := nest - 1;
    loopfactor := loopfactor - 1;
  end {repeatstatement} ;
  {>>>}
  {<<<}
  procedure forstatement;
  {<<<}
  { Syntactic routine to parse a for statement:
    for-statement = "for" variable ":=" expression
          ( "to" | "downto" ) final-value "do" statement  .

    The controlled variable must be an entire variable local to the
    current block.  Within the for statement, the controlled variable must
    not be used in any context where it can be modified.  Also, the
    for variable has an undefined state if the for loop exits normally.

    Since it is also highly desirable to keep the controlled variable
    in a register for the duration of the loop, controlled variables
    are handled in a special manner.

    The controlled variable is placed in a stack, and as long as it
    is on that stack it will be handled specially.  Also, for limits
    are passed to travrs differently if they are constant than if
    they are expressions.
  }
  {>>>}
  {<<<}
  var
    oldcheck: boolean; {old value of checkundefs}
    oldjumpoutnest: integer; {old value of jumpoutnest}
    upflag: boolean; {true if this if "for" "to"}
    forlen: addressrange; {length of controlled var}
    localflag: boolean; {true if var unused by interior procs}

    t: forstackindex; {used for searching for stack}
    forvar: tableIndex; {index of controlled var}
    forvarptr: entryptr; {provides access to cont. var entry}
    fortype: tableIndex; {type of for var}
    fortypeptr: entryptr; {for access to fortype data}

    initconst: boolean;   { initial value constante}
    initout: boolean;     { intial out of range}
    initlcheck, inithcheck: boolean; {set if low or high needs checking}
    initcol: columnindex; { column of initial expression}
    initline: integer;    { text line of initial expression}
    initval: integer;     { initial constant value}
    usinitval: unsignedint; { unsigned version of initial constant value}
    initrange: range;     { range of initial value}

    finallen: integer;    { length of final value}
    finalconst: boolean;  { final value is constant}
    finalval: integer;    { final constant value}
    usfinalval: unsignedint; { unsigned version of final constant value}
    finalout: boolean;    { final value out of range}
    finlcheck, finhcheck: boolean; { set if low or high needs checking}
    finalrange: range;    { range of final value}

    lowerbound, upperbound: integer; {for type range}
    extendedfor: boolean; {an extended range for statement}
    unsignedfor: boolean; {an unsigned for statement}
  {>>>}

  begin
    loopfactor := loopfactor + 1;
    getexprstmt(begfor);
    forvar := 0;
    fortype := noneindex;
    lowerbound := 0;
    upperbound := 0;
    unsignedfor := false;
    extendedfor := false;

    if token = ident then
      {<<<  ident}
      begin
      search (forvar);

      if forvar = 0 then
        warn(undefidenterr)
      else
        begin
        if checkforstack(forvar, t) then
          warn (modifiedfor);

        forvarptr := ref(bigtable[forvar]);
        with forvarptr^ do
          if namekind in [varname, param, varparam] then
            begin
            fortypeptr := ref(bigtable[vartype]);

            { We don't support for loop indexes that are origined, declared USE, DEFINE or SHARED, or OWN.
              OWN is allowed if the global section is not split }
            if ((varalloc = ownalloc) and (sharedPtr^.globalsize > sharedPtr^.globalfiles)) or
               (varalloc in [absolute, usealloc, definealloc, sharedalloc]) then
              warn (unsupportedforvardecl);

            if not (fortypeptr^.typ in [none, ints, chars, scalars, bools, subranges]) then
              warn(badfortype)
            else
              begin
              fortype := vartype;
              lowerbound := lower(fortypeptr);
              upperbound := upper(fortypeptr);
              extendedfor := fortypeptr^.extendedrange;
              unsignedfor := unsigned(fortypeptr, length, false);
              end;

            if (namekind <> varname) or (lev <> level) then
              warn (badforvar)
            else if nestedmod then
              warnnonstandard (badfornestref);

            forlen := length;
            localflag := registercandidate and ((level > 1) or not anyexternals);
            end
          else
            warn (wantvarname);
        end;

      variable (true, true, false, true, true, forvar);
      end
      {>>>}
    else
      warnbetween (missingforindex);

    if token = eql then
      begin
      warn (nobecomeserr);
      gettoken
      end
    else
      begin
      verify ([becomes], follow + begexprset, nobecomeserr);
      if token = becomes then
        gettoken;
      end;

    expression (follow + [downtosym, tosym, untilsym, dosym], false);
    if not compatible (fortype, resulttype) then
      warnbefore (badforlimit);

    oprndstk[sp].typeindex := fortype;
    initconst := constcheck (sp);
    if initconst then
      initval := getintvalue(sp);

    checkrange (oprndstk[sp], false, initout, initlcheck, inithcheck);
    initrange := oprndstk[sp].value_range.optimistic;
    if initout then
      with lasttoken do
        begin
        initcol := (left + right) div 2;
        initline := line;
        end;

    if initconst then
      begin
      genlit (initval);
      sp := sp - 1;
      if unsignedfor then
        genop (defunsforlitindexop)
      else
        genop (defforlitindexop)
      end
    else
      begin
      genoprnd;
      if unsignedfor then
        genop (defunsforindexop)
      else
        genop (defforindexop)
      end;

    genint (forlen);
    genint (ord(localflag));

    upflag := (token = tosym);
    if token in [downtosym, tosym] then
      gettoken
    else
      warnbetween(nodowntoerr);
    expression(follow + [dosym], false);

    if not compatible (fortype, resulttype) then
      warnbefore (badforlimit);

    finalconst := constcheck(sp);
    oprndstk[sp].typeindex := fortype;
    oprndstk[sp].oprndlen := max(forlen, oprndstk[sp].oprndlen);
    if finalconst then
      begin
      finallen := forlen;
      finalval := getintvalue(sp)
      end
    else
      begin
      finallen := oprndstk[sp].oprndlen;
      genunary(pushfinal, ints);
      end;

    finalrange := oprndstk[sp].value_range.optimistic;
    oprndstk[sp].typeindex := fortype;
    checkrange (oprndstk[sp], false, finalout, finlcheck, finhcheck);

    genoprnd;
    if finalconst and initconst then
      begin
      usinitval := initval;
      usfinalval := finalval;
      if upflag and
         (extendedfor and (usinitval <= usfinalval) or not extendedfor and (initval <= finalval)) or not upflag and
         (extendedfor and (usinitval >= usfinalval) or not extendedfor and (initval >= finalval)) then
        begin
        if initout then
          warnat (rangeerror, initline, initcol);
        if finalout then
          warnbefore (rangeerror);
        end;
      end
    else if (sharedPtr^.switchcounters[rangecheck] > 0) and (upflag and (initlcheck or
            finhcheck) or not upflag and (inithcheck or finlcheck)) then
      begin
      if initout or finalout then
        begin
        genlit (ord(not upflag));
        genop (forerrchkop);
        end
      else
        begin
        genlit (lowerbound);
        genlit (upperbound);
        if upflag then
          genop(forupchkop)
        else
          genop (fordnchkop);
        end;
      genint (finallen);
      genint (1);
      genform (ints);
      end;

    genop (endexpr);
    intstate := stmtstate;
    if upflag then
      genstmt (forup)
    else
      genstmt (fordn);
    genint (1);

    verifytoken (dosym, nodoerr);
    forsp := forsp + 1;
    with forstack[forsp] do
      begin
      containedgoto := false; {hopefully remains false!}
      forindex := forvar;
      fortypeptr := ref(bigtable[fortype]);
      settyperange(fortypeptr, forrange);
      if upflag then
        begin
        if finalrange.maxlimit >= initrange.minlimit then
          begin
          if not finhcheck then
            forrange.maxlimit := finalrange.maxlimit;
          if not initlcheck then
            forrange.minlimit := initrange.minlimit;
          end
        end
      else
        begin
        if finalrange.minlimit <= initrange.maxlimit then
          begin
          if not finlcheck then
            forrange.minlimit := finalrange.minlimit;
          if not inithcheck then
            forrange.maxlimit := initrange.maxlimit;
          end
        end;
      end;

    forvarptr := ref(bigtable[forvar]);
    with forvarptr^ do
      if namekind in [varname, fieldname, param, varparam] then
        modified := true;

    oldcheck := checkundefs;
    checkundefs := false;
    nest := nest + 1;
    oldjumpoutnest := jumpoutnest;
    jumpoutnest := nest;
    statement(follow);
    checkundefs := oldcheck;
    updatelabelnest;
    genstmt (endfor);
    genint (ord(jumpoutnest < nest));
    jumpoutnest := min(jumpoutnest, oldjumpoutnest);
    nest := nest - 1;
    loopfactor := loopfactor - 1;

    if not (forstack[forsp].containedgoto and
       (sharedPtr^.switchcounters[standard] > 0)) and (nest = 1) then
      begin
      forvarptr := ref(bigtable[forvar]);
      if forvarptr^.namekind = varname then forvarptr^.modified := false;
      end;
    forsp := forsp - 1;
  end;
  {>>>}
  {<<<}
  procedure withstatement;
  { Syntactic routine to parse a with-statement.
    with-statement = "with" variable [* ";" variable *] "do"
          statement  .

    As each variable is parsed, the scope id is pushed onto the display
    to let its fields be found in the normal search sequence.  These are
    popped at the end of the statement.  Also, the address of the record
    is written to the intermediate file.
  }
  var
    withcount: integer; {number of variables}
    i: integer; {induction variable}

    {<<<}
    procedure onewith;
    { Parse a single variable in a with statement.  This actually pushes the
      display and generates the output.
    }

      var
        p: entryptr; {access with variable}
        off: addressrange; {offset of with variable}
        i: tableIndex; {index of with variable}
        l: levelindex; {level of with variable}


      begin {onewith}
        newexprstmt(begwith);
        if token = ident then
          begin
          search(i);
          p := ref(bigtable[i]);
          if p^.namekind in [varname, fieldname, param, varparam] then
            off := p^.offset
          else off := 0;
          l := lev;
          modifyvariable(true, true);
          genlit(0);
          genunary(indxop, ints);
          if resultform = fields then
            if displaytop < maxlevel then
              begin
              displaytop := displaytop + 1;
              with display[displaytop] do
                begin
                scopeid := display[displaytop - 1].scopeid;
                blockid := resultptr^.fieldid;
                blockkind := withblock;
                withpacking := resultptr^.packedflag;
                withoffset := off;
                withlevel := l;
                end;
              withcount := withcount + 1;
              genoprndstmt;
              end
            else warnbefore(levelerr)
          else if resulttype <> noneindex then warnbefore(norecordident)
          end
        else warnbetween(norecordident);
      end {onewith} ;
    {>>>}

  begin
    gettoken;

    withcount := 0;
    onewith;
    while token in [comma, ident] do
      begin
      verifytoken (comma, nocommaerr);
      onewith;
      end;

    verifytoken (dosym, nodoerr);
    statement (follow);

    displaytop := displaytop - withcount;
    for i := withcount downto 1 do
      genstmt (endwith);
  end;
  {>>>}
  {<<<}
  procedure gotostatement;
  { Syntactic routine to parse a goto statement.
    goto-statement = "goto" label  .

    This also has to check if the goto is legal, though if the
    label is not yet defined this may have to wait for the label to
    be defined.
  }
  var
    lab: labelptr; {label entry}
    gotoline: integer; {line on which goto appeared}
    gotofilepos: integer; {file position of goto token}
    gotofileIndex: integer; {fileindex of goto token}

  begin {gotostatement}
    forstack[forsp].containedgoto := true;
    gotoline := thistoken.line;
    gotofilepos := thistoken.filepos;
    gotofileIndex := thistoken.fileIndex;
    gettoken;
    if token = intconst then
      begin
      searchlabels(thistoken.intvalue, lab);
      with lab^ do
        begin
        if lab = labelflag then warn(labnotpredef)
        else
          begin
          if definednest = 0 then maxlegalnest := min(nest, maxlegalnest)
          else if (nest > maxlegalnest) or (nest < definednest) then
            warnat(badlabelnest, labelline, labelcolumn)
          else if (nest > definednest) and (lev = level) then
            jumpoutnest := min(jumpoutnest, definednest);
          if lev <> level then
            begin
            nonlocalref := true;
            maxlegalnest := 1;
            end;
          end;
        if (lev > 1) and (lev <> level) then
          sharedPtr^.proctable[display[level].blockref].intlevelrefs := true;
        debugstmt(gotolab, gotoline, gotofilepos, gotofileIndex);
        genint(internalvalue);
        genint(lev);
        end;
      gettoken
      end
    else warnbetween(badlabelerr);
  end {gotostatement} ;
  {>>>}

begin {statement}
  sp := - 1;

  if token = intconst then
    assignlabel;

  if token in [intconst, charconst, realconst, dblrealconst, stringconst] then
    illegalassign

  else if token = ident then
    begin
    search (varindex);
    if varindex = 0 then
      illegalassign
    else
      begin
      varptr := ref(bigtable[varindex]);
      case varptr^.namekind of
        standardproc: standardprocedures(varptr^.procid);
        varname, fieldname, param, varparam, confparam, varconfparam:
          {<<<}
          begin
          newexprstmt(simple);
          assignment;
          end;
          {>>>}
        forwardproc, externalproc, procname:
          {<<<}
          begin
          newexprstmt(simple);
          procedurecall(varindex);
          genoprndstmt
          end;
          {>>>}
        procparam:
          {<<<}
          begin
          newexprstmt(simple);
          paramcall(varindex);
          genoprndstmt
          end;
          {>>>}
        funcname:
          {<<<}
          begin
          varptr^.funcassigned := true;
          newexprstmt(simple);
          if (lev >= level) or (varindex <> display[lev + 1].blockname) then
            begin
            warn(badfuncassign);
            illegalassign
            end
          else assignment;
          end;
          {>>>}
        otherwise
          illegalassign
        end;
      end;
    end

  else
    case token of
      beginsym:
        compoundstatement;
      ifsym:
        ifstatement;
      casesym:
        casestatement;
      whilesym:
        whilestatement;
      repeatsym:
        repeatstatement;
      forsym:
        forstatement;
      withsym:
        withstatement;
      gotosym:
        gotostatement;
      elsesym:
        if not (elsesym in follow) then
          badelseclause;
      otherwisesym:
        if not (otherwisesym in follow) then
          badelseclause;
      otherwise;
      end;
end;
{>>>}

{<<<}
procedure body;
{ Syntactic routine to parse a block.
  Productions:
  body = "begin" statement [* ";" statement *] "end"
  This is broken out as an external procedure to allow overlaying
  code with the declaration processing code which is contained
  in "analys".
}

  begin {body}
    if sharedPtr^.switcheverplus[defineswitch] then
      warn (bodyfounderr);

    nest := 1;
    jumpoutnest := maxint;
    nolabelsofar := true;

    if token in [ifsym..gotosym, ident] then
      warn (nobeginerr)
    else
      verifytoken (beginsym, nobeginerr);

    statement (neverskipset);
    while not (token in [labelsym..functionsym, endsym, eofsym]) do
      begin
      if token = semicolon then
        gettoken
      else
        verify1([semicolon], nosemierr);
      statement (neverskipset);
      end;

  end {body};
{>>>}
{>>>}

{ forwards }
procedure block; forward;
function create_type (typeindex: tableIndex) : unsignedword; forward;
procedure gettyp (follow: tokenset; var resulttype: tableIndex); forward;

{<<<}
procedure enterblock (level: levelindex; bn: tableIndex; ref: proctableindex);
{ Set up the display for a new block.  This is called prior to parsing
  any definitions for the new block, including parameters.  Most of
  the display is initialized to empty values.  The scope id is incremented for the new block. }

begin
  displaytop := level;
  sharedPtr^.blockref := ref; {global for 'panic' routine}

  with display[level] do
    begin
    if (lastid >= totalscopes) or (lastscope >= totalscopes) then
      analysFatal (manyscopes); {the last value of totalscopes is not used.}
    lastid := lastid + 1;
    blockid := lastid;
    lastscope := lastscope + 1;
    scopeid := lastscope;
    dbgscope := 0;
    firststmt := 0;
    laststmt  := 0;
    blockref := ref;
    blockkind := codeblock;
    blocksize := 0;
    paramsize := 0;
    oldundeftabletop := undeftabletop;
    labellist := labelflag;
    threshold := tabletop;
    blockname := bn;
    highestkey := 0;
    namesdeclared := 0;
    sharedPtr^.proctable[blockref].opensfile := false;
    end
end;
{>>>}
{<<<}
procedure fixupparamoffsets (endofdefs: boolean);
{<<<}
{ Parameters are allocated before variables but are addressed
  relative to the stack pointer.  This means that after a variable-
  declaration-part is parsed the parameter offsets must be adjusted
  to reflect any new variables allocated in this block.

  "Fixupparamoffsets" scans the parameters and makes necessary adjustments
  to the offsets.

  This routine assumes that parameters are allocated name
  entries immediately following the block name entry.

  For nonpascal routines (routines accessable via C or F77), parameter
  offsets are reversed since prior to being called from C they will
  have been pushed on the stack in reverse order.

  Nonpascal/Fortran type routine must reserve extra space in the
  local data area to store the function return value.

  If we are compiling for MS-windows, then we increment the blocksize
  with a word to reflect the saved datasegment.  The blocksize
  must be used (and not the returnlinksize), since all mapping to
  frame pointer relative offsets, including mapping for the
  debugger, use blocksize.
}
{>>>}
{<<<}
var
  i: tableIndex; {induction var}
  t: tableIndex; {last parameter index}
  bn: tableIndex; {block name index}
  p: entryptr; {used for accessing procedure and parameters}
  newoffset: addressrange; {dummy to fix code gen bug on the PDP-11}
  startoffset: addressrange; {start of param offsets for non pascal; vms only;
                             it is four when the function returns something
                             huge; (well greater than quad, anyway)}
{>>>}

begin
  with display[level] do
    begin
    bn := blockname;
    if endofdefs then
      begin
      blocksize := forcealign (blocksize + paramsize, stackalign, false) - paramsize;
      if level = 1 then
        sharedPtr^.ownsize := forcealign (sharedPtr^.ownsize, sharedPtr^.targetintsize, false);
      end;

    p := ref(bigtable[bn]);

    t := p^.paramlist;
    { Fortran declared functions will have an extra parameter
      pushed that points to the function return value. }
    if (sharedPtr^.proctable[blockref].calllinkage = fortrancall) and
       (sharedPtr^.proctable[blockref].registerfunction = 0) and
       (p^.functype <> noneindex) then
      paramsize := paramsize + wordsize;

    i := bn + 1;
    while i <= t do
      begin
      p := ref(bigtable[i]);
      if not p^.form then
        begin
        { Non-Pascal declared routines will have reversed parameters. }
        if (sharedPtr^.proctable[blockref].calllinkage = nonpascalcall) then
          newoffset := blocksize + p^.offset
        else {pascal2call/fortrancall}
          newoffset := paramsize + blocksize - forcealign(p^.offset + p^.length, stackalign, false);

        if sharedPtr^.proctable[blockref].externallinkage then
          newoffset := newoffset + sharedPtr^.extreturnlinksize
        else
          newoffset := newoffset + sharedPtr^.returnlinksize;

        p^.offset := newoffset;
        if p^.namekind in [procparam, funcparam] then
          i := p^.nextparamlink;
        end;
      i := i + 1;
      end;
    end {with display};
end;
{>>>}
{<<<}
procedure exitblock (level: levelindex);
{ Called to exit the block at "level".
  All forms defined in the block are disposed of,
  and any names declared in the block are removed from the name table and the key map.
  Finally, the block id and table tops are reset to the value on entry to the block.
}
{<<<}
var
  t: tableIndex; {top of debug file}
  i: tableIndex; {Induction variable}
  p: entryptr; {used for access to names}
  regok: boolean; { true if we can assign vars to registers in this block }
  localvar: localvartype; { for writing to file }
{>>>}

begin
  { Since for UMAX our global registers cannot overlap Unix-destroyed registers, 
    there is no need to restrict global allocation based on nonpascal references. }
  regok := not anynonpascalcalls and not anynonlocallabels and (not anyexternals or (level > 1));

  tempvars := 0;

  with display[level] do
    begin
    genint(0);
    for i := oldtabletop + 1 to tabletop do
      begin
      p := ref(bigtable[i]);
      if not p^.form and (p^.namekind = varname) then
        begin
         { tell travrs about any var that may be assigned to a register }
         if regok and p^.registercandidate then
           possibletemp (p^.offset, p^.vartype, p^.dbgsymbol);
         end;
      end;

    { signal the end of local vars for this block }
    localvar.typ := none;
    write (sharedPtr^.localFile, localvar);
    lastid := blockid - 1;

    { Now remove entries from the keymap,  all entries with
      name greater than the name at start of block are new.
      This is unnecessary at the global level, as we'll do nothing
      other than move on to the next compilation phase!
      well, we still need to take care of the symbol table: don't output it twice.. }
    if (level > 1) or sharedPtr^.switcheverplus[symboltable] then
      begin
      while namesdeclared > 0 do
        begin
        i := highestkey;
        highestkey := highestkey - 1;
        t := keymap[i];
        while t > threshold do
          begin
          namesdeclared := namesdeclared - 1;
          p := ref(bigtable[t]);
          t := p^.nextname;
          end;
        keymap[i] := t;
        end;

      { If parameters, oldtabletop has been fudged}
      tabletop := oldtabletop;
      undeftabletop := oldundeftabletop;
      end;
    end;
end;
{>>>}

{<<<}
procedure listundefprocs;
{ Examine the table of forward defined procedures and emit an error message
  for all which are still undefined.  Also emits errors for program
  parameters which are never defined in the variable section }

var
  p, pp: entryptr; { used for access to name table entry }
  i: undefindex; { induction var for table scan }

begin
  for i := display[level].oldundeftabletop + 1 to undeftabletop do
    begin
    p := ref(bigtable[undeftable[i].tableref]);
    with p^, undeftable[i] do
      if form then
        begin
        { the only way a "form" entry gets put in this table is
          when generating tables for the debugger, and we need to
          update the type a pointer points to, after that type is
          known.  The "dbgsymbol" testing code is just bulletproofing. }
        end
      else if namekind in [forwardproc, forwardfunc] then
        warnat (fwdundef, line, column)
      else if namekind = undefname then
        warnat (nameundef, line, column);
    end;
end;
{>>>}
{<<<}
procedure listundeftypes;
{ Examine the table of forward defined types, emiting an error message for all which are still undefined }

var
  p: entryptr; {used for access to name table entry}
  i: undefindex; {induction var for table scan}

begin
  for i := display[level].oldundeftabletop + 1 to undeftabletop do
    begin
    p := ref(bigtable[undeftable[i].tableref]);
    with undeftable[i] do
      begin
      if p^.namekind = undeftypename then
        begin
        with p^ do
          if namekind = undeftypename then
            begin
            if (typeindex = noneindex) then
              warnat(typeundef, line, column);
            namekind := typename;
            end;
        end;
      end;
    end;
end;
{>>>}
{<<<}
procedure listundeflabels;
{ check the list of labels declared in the block just completed and emit an error message for any not defined }

var
  p: labelptr; { used for tracing the label list }

begin
  p := display[level].labellist;
  while p <> labelflag do
    with p^ do
      begin
      if definednest = 0 then warnat(labelundef, labelline, labelcolumn);
      p := nextlabel;
      end;
end;
{>>>}

{<<<}
procedure enterundef (where: tableIndex);
{ Enter an undefined name (pointer type or forward proc or func) in
  the undeftable.  This is used to make sure that all forward defs
  are satisfied by the end of the block.  The line and column are
  saved for an error message.
}
begin
  undeftabletop := undeftabletop + 1;
  if undeftabletop > undeftablesize then
    analysFatal (undeftablefull);
  with undeftable[undeftabletop] do
    begin
    tableref := where;
    line := lasttoken.line;
    column := (lasttoken.left + lasttoken.right) div 2;
    end;
end;
{>>>}
{<<<}
function newproc: proctableindex;
{ Allocates a new entry in the proctable, initializes its entries,
  and returns the index of the new entry.
}
begin
  sharedPtr^.proctabletop := sharedPtr^.proctabletop + 1;

  if sharedPtr^.proctabletop > proctablesize then
    analysFatal (proctablefull);

  with sharedPtr^.proctable[sharedPtr^.proctabletop] do
    begin
    charindex := 0;
    charlen := 0;
    globaldeath := false;
    externallinkage := false;
    any_calls := false; {unused by P-2}
    struct_calls := false;
    struct_ret := false;
    bodydefined := false;
    intlevelrefs := false;
    opensfile := false;
    realfunction := false;
    isprocparam := false;
    referenced := true;
    ownused := false;
    calllinkage := pascal2call;
    registerfunction := 0;
    backlink := 0; {to be complete}
    level := 1;
    levelspread := 0;
    end;

  newproc := sharedPtr^.proctabletop;
end;
{>>>}

{<<<}
{ Symbol Table and Display Structure

  The symbol table and display work together to implement Pascal's name access rules.
  They are only loosely connected, unlike many Pascal compilers where they are part of the same structure.

  Each unique identifier is assigned a unique number called a "key" by the source scanner.
  These keys are used as an index into a keymap table which relates the key to a name table entry.
  However, in Pascal, the same name may refer to different objects in different scopes.
  The compiler assigns unique identifiers to each scope, and the combination of scope id and key
  is sufficient to identify an object.

  The symbol table contains a separate name entry for each object,
  and all objects with the same key are linked on a chain rooted in the keymap.
  The scope id is stored in the name entry (called "name" of all things) and is used when searching for a particular object.
  Thus the symbol table provides a complete mapping from scope id and key to a name entry.

  The display, on the other hand, keeps track of currently accessable scopes.
  Thus, upon entering a scope, the display is assigned an id, and this is used for all items defined within this scope.
  Each procedure and each record will have it's own scope id.
  Only those scopes which are represented on the display will normally be accessable.
  Thus, to implement a "with" it is only necessary to make a display entry containing the scope id of the record type,
  and The fields of the record may be found by searching for an identifier with the record's scope id.

  This approach completely decouples accessing rules from the symbol table.
  The symbol table handles names, and the display handles access rules.

  There is an optimization which conserves space in the symbol table, as well as speeding access.
  Since identifiers defined within a block are inaccessable after exit from that block,
  they can be removed from the symbol table at block exit.
  This is not the case for record fields, or for parameters.

  Record fields are simply left in the symbol table, and parameters are removed from the keymap chain,
  but their entries are left in the name table.
  They are no longer accessable through the key map, but can be found from the procedure entry.

  Parameters in forward definitions are treated somewhat differently, for practical reasons.
  They have their scope id set to an impossible value to make them inaccessable.

  In addition, scope id's can be re-used after exit from a block, thus conserving space in the symbol table.
}
{>>>}
{<<<}
procedure searchlevel (var wherefound: tableIndex);
{ Search the current level for the current token.  The result is returned in "wherefound".  If not found, returns zero }

begin
  searchsection (display[level].blockid, wherefound);
end;
{>>>}

{ Name Table Entry Procedures - Procedures to enter identifiers into the name table }
{<<<}
procedure enterident (id: integer; var newindex: tableIndex; undefok: nametype);
{<<<}
{ Enter the current token as an identifier with scope id "id".  The
  resulting name index is returned in "newindex".  If undefok is set to
  be undefname or undeftypename, this could be a forward type definition
  from a prior pointer declaration, or a program statement parameter,
  and namekind must equal undefok. Other than this special case, if the
  identifier is already defined in this scope or has been used in this
  scope an error message is emitted.  If the token is not an identifier,
  an error message to that effect is emitted.
}
{>>>}

{<<<}
var
  key: integer; {identifier key from scanner}
  p: entryptr; {used for access to name entry}
  t: tableIndex; {used for name search}
{>>>}

begin
  newindex := 0;
  probing := true;
  if token = ident then
    begin
    key := thistoken.key;
    search(t);
    if id <> display[displaytop].blockid then
      begin
      p := ref(bigtable[t]);
      if (t <> 0) and (p^.namekind in [undefname, undeftypename]) and
         (p^.lastoccurrence >= lastscope) then
        warn(duplicateident);
      searchsection(id, t);
      end;
    if t <> 0 then
      begin
      p := ref(bigtable[t]);
      if sharedPtr^.switcheverplus[multidef] and
         (p^.namekind = varname) and
         (p^.varalloc in [usealloc, definealloc]) then newindex := t
      else
        begin
        if (p^.lastoccurrence >= display[displaytop].scopeid) and
           (p^.name <> id) or (p^.name = id) and
           (undefok <> p^.namekind) then warn(duplicateident);
        if (p^.name = id) and (p^.namekind = undefok) then newindex := t;
        end;
      end;
    end
  else
    key := 0;
  probing := false;

  if newindex = 0 then
    begin
    if tabletop = tablesize then
      analysFatal (tablefull)
    else
      tabletop := tabletop + 1;
    display[displaytop].namesdeclared := display[displaytop].namesdeclared +
                                         1;
    if key > display[displaytop].highestkey then
      display[displaytop].highestkey := key;
    p := ref(bigtable[tabletop]);
    p^.dbgsymbol := 0;
    p^.form := false;
    p^.nextname := keymap[key];
    p^.name := id;
    p^.namekind := noname;
    p^.typeindex := noneindex;
    p^.refdefined := false; {!!!}
    p^.lastoccurrence := lastscope;
    p^.vartype := noneindex;
    p^.sparelink := 0; { pdb kluge, see dumpname }
    p^.varalloc := normalalloc; { initialization needed for $multidef }
    if token = ident then
      begin
      p^.charindex := thistoken.keyPos;
      p^.charlen := thistoken.right - thistoken.left + 1;
      end
    else
      begin
      p^.charindex := 0;
      p^.charlen := 0;
      end;

    keymap[key] := tabletop;
    newindex := tabletop;
    end;

  verifytoken (ident, novarerr);

end;
{>>>}
{<<<}
procedure enterlocalident (var newindex: tableIndex; undefok: nametype);
{ Enter current token as identifier in local scope }

begin
  enterident (display[level].blockid, newindex, undefok);
end;
{>>>}

{<<<}
procedure programheading;
{ Syntactic routine to parse a program-heading.
  program-heading = "program" identifier [ "(" identifier [* "," identifier *] ")" ] ";"
  This is simply checked for syntactic correctness and ignored
}
  {<<<}
  procedure oneprogramparam;
  { Process one program parameter.  All identifiers declared in the
    program header must be later declared as variables at the global
    level.  The only exception is input and output, which are already
    defined implicitly.  There is an interaction with files: all external
    files must be declared in the program statement.  However, program
    parameters need not be files.  We always give an error if a program
    parameter appears but is never declared as a variable, however the
    external file restriction is only enforced if the program is compiled "standard". }

  var
    identindex: tableIndex; {index to program parameter, if it exists}
    idenp: entryptr;        {used to refer to name entry}

  begin
    if token = ident then
      begin
      searchlevel (identindex);

      if identindex = 0 then
        begin
        enterlocalident (identindex, noname);
        enterundef (identindex);
        idenp := ref (bigtable[identindex]);
        idenp^.namekind := undefname;
        end
      else
        begin
        idenp := ref (bigtable[identindex]);
        if idenp^.programdecl then
          warn(duplicateident)
        else if identindex = inputindex then
          inputdeclared := true
        else if identindex = outputindex then
          outputdeclared := true
        else
          warn(compilerwritererr);
        gettoken;
        end;

      idenp^.programdecl := true;
      end

    else
      begin
      warnbetween (novarerr);
      gettoken;
      end;
  end;
  {>>>}

begin
  gettoken;

  sharedPtr^.proctable[0].charindex := thistoken.keyPos;
  sharedPtr^.proctable[0].charlen := min(maxprocnamelen, thistoken.right - thistoken.left + 1);
  verifytoken (ident, novarerr);

  if token = lpar then
    begin
    gettoken;
    oneprogramparam;
    while token in [comma, ident] do
      begin
      verifytoken (comma, nocommaerr);
      oneprogramparam;
      end;

    verifytoken (rpar, norparerr);
    end;

  verifytoken (semicolon, nosemierr);
end;
{>>>}
{<<<}
function getdbgsymbol (i: tableIndex): p_symbolindex;
{ Map a compiler index to debugger record index when we know an entry has already been made, as in a name. }

var
  p:entryptr; {The usual}

begin
  if i = 0 then
    getdbgsymbol := 0
  else
    begin
    p := ref(bigtable[i]);
    getdbgsymbol := p^.dbgsymbol;
    end;
end;
{>>>}

{<<<}
procedure conststructure (follow: tokenset; form: tableIndex; var value1: operand);
{<<<}
{ Syntactic routine to parse a constant structure.
  Productions:
  structured-constant = type-identifier structured-value  .
  structured-value = "(" constant-element [* "," constant-element *]

  This routine checks for a legal type, and parses arrays with
  one routine and records with another.

  The constant value is buffered as it is generated, and is written
  to the string file if the size cannot be represented as an integer.

  NOTE:
    In order to understand what is going on in the handling of constants
    that are being treated as integers (representation = ints), it is
    necessary to be aware of two inviolate rules:

    (1) All integer constant elements are kept in HOST form within
        the compiler.  They are changed to target form, if there is
        a difference, only when they are actually written to the
        constant buffer.

    (2) If a value is smaller than an integer, the value is kept in
        the low order bytes of the integer.  For high-to-low packing,
        this implies that the internal representation of the last or
        only portion of a packed value must be shifted down to the
        low order as soon as the constant is complete.

  The values of "reversebytes" and "hostintlowbytefirst" (defined in
  config) determine the transformations that must take place, if any.

  In some places, there are tests such as:
    if hostintlowbytefirst <> reversebytes then ...
  This test, if true, says that TARGET integers have the low byte first.

  The possibilities are:
    hostintlowbytefirst  reversebytes    Implications
  =               False         False    Target has high byte first
  <>              False          True    Target has low byte first
  <>               True         False    Target has low byte first
  =                True          True    Target has high byte first
}
{>>>}

const
  cbufsize = 32; {constant buffering, must be big enough to cover a real and an integer}

type
  constbuffer = {holds and converts constant values}
    record
      case integer of
        1: (b: packed array [1..cbufsize] of hostfilebyte);
        2: (i: integer);
        3: (r: realarray {double} );
        4: (p: integer {targep} );
    end;

var
  cbuf: constbuffer; {holds bytes to write}
  cbytes: 0..cbufsize; {current bytes in cbuf}
  curloc: addressrange; {current location relative to start of constant}
  baseloc: addressrange; {start of packed buffers}
  pbuf1, pbuf2: integer; {packed data buffers}
  f: entryptr; {for access to form data}
  packedflag: boolean; {this is a packed form}

  {<<<}
  procedure alignpartialint (var int: integer; {integer to be aligned}
                            bytes: addressrange {actual size in use} );
  { If the value is in the high order end of the integer, and it hasn't
    used all bytes of the integer, we need to slide the value down to the
    low order end, consistent with the rule that all values are kept in the low order end of the integer.
  }
  var
    kludge: constbuffer; {used to shift left or right}
    dist: 0..cbufsize; {distance to shift}
    i: 0..cbufsize; {induction on bytes in integer}

  begin
    if (bytes < hostintsize * hostfileunits) and (bytes > 0) then
      begin
      kludge.i := int;
      dist := hostintsize * hostfileunits - bytes;

      if hostintlowbytefirst then
        begin {shift left}
        for i := 1 to bytes do kludge.b[i] := kludge.b[i + dist];
        for i := bytes + 1 to hostintsize * hostfileunits do
          kludge.b[i] := 0;
        end
      else
        begin {shift right}
        for i := bytes downto 1 do kludge.b[i + dist] := kludge.b[i];
        for i := dist downto 1 do kludge.b[i] := 0;
        end;
      int := kludge.i;
      end;
  end;
  {>>>}
  {<<<}
  procedure reversestructure (var s: constbuffer; {structure to munge}
                             bytes: integer {number of bytes to flip} );
  { Reverse the bytes within each integer of a structure.
  }

  var
    i, j: 1..cbufsize; {induction on bytes of an integer}
    k: 0..cbufsize; {offset of integer in constant buffer}
    t: hostfilebyte; {temp for flipping structure}

  begin
    if reversebytes then
      begin
      k := 0;

      while bytes - k > 0 do
        begin {reverse the active bytes in each integer}
        i := k + 1;
        j := k + hostintsize * hostfileunits;
        if bytes - k < hostintsize * hostfileunits then
          if hostintlowbytefirst then
            j := bytes {do left part}
          else
            i := j + 1 - (bytes - k); {do right part}

        while i < j do
          begin
          t := s.b[i];
          s.b[i] := s.b[j];
          s.b[j] := t;
          i := i + 1;
          j := j - 1;
          end;
        k := k + hostintsize * hostfileunits;
        end;
      end;
  end {reversestructure} ;
  {>>>}

  {<<<}
  procedure putcbyte (b: hostfilebyte {constant byte to put} );
  { Put a byte to the constant buffer, writing the buffer to the string file if it is full. }

  var
    i: 1..cbufsize;

  begin
    if emitflag then
      begin
      if cbytes = cbufsize then
        begin
        seekstringfile (sharedPtr^.stringfilecount);
        for i := 1 to cbufsize do
          putbyte (cbuf.b[i]);
        cbytes := 0;
        end;
      cbytes := cbytes + 1;
      cbuf.b[cbytes] := b;
      end;
  end;
  {>>>}
  {<<<}
  procedure putcint (int: integer; {integer to write}
                    bytes: addressrange {bytes to write} );
  { Put "bytes" bytes of an integer to the constant buffer.
    If there are more bytes specified than in an integer, empty bytes are appended to the end.
  }
  var
    kludge: constbuffer; {used to separate bytes of the integer}
    datasize: 0..cbufsize; {number of bytes of actual integer}
    i: addressrange; {induction on "bytes"}

  begin
    if emitflag then
      begin
      kludge.i := int;
      datasize := min(bytes, hostintsize);

      if reversebytes then
        reversestructure (kludge, hostintsize * hostfileunits);

      if hostintlowbytefirst <> reversebytes {low order first} then
        for i := 1 to datasize do
          putcbyte (kludge.b[i])
      else {high order first}
        for i := hostintsize * hostfileunits + 1 - datasize to hostintsize * hostfileunits do
          putcbyte (kludge.b[i]);
      for i := hostintsize * hostfileunits + 1 to bytes do
        putcbyte(0);
      end;
  end;
  {>>>}
  {<<<}
  procedure putcreal (r: realarray; {double} size: integer);
  { Put a real value to the constant buffer. }

  var
    kludge: constbuffer; {used to separate the bytes of the real}
    i: 1..cbufsize; {induction on words of the real number}
    t: hostfilebyte; {temp for switching bytes}

  begin
    kludge.r := r;

    if reversebytes then
      for i := 1 to (size * hostfileunits) div 2 do
        begin
        t := kludge.b[i * 2 - 1];
        kludge.b[i * 2 - 1] := kludge.b[i * 2];
        kludge.b[i * 2] := t;
        end;

    for i := 1 to size * hostfileunits do
      putcbyte (kludge.b[i]);
  end;
  {>>>}
  {<<<}
  procedure putcptr (p: integer {targep} {pointer to generate} );
  { Put a pointer value to the constant buffer.  Pointer constants are
    kept in target form, so there is no need to call reversestructure.
    NOTE: This routine assumes that a pointer will never be smaller than an integer
  }
  var
    kludge: constbuffer; {used to separate the bytes of the pointer}
    i: 1..cbufsize; {induction on bytes of the pointer}

  begin
    for i := 1 to sharedPtr^.ptrsize * hostfileunits do
      kludge.b[i] := 0;
    kludge.p := p;

    if hostintsize < sharedPtr^.ptrsize then
      begin
      if hostintlowbytefirst = reversebytes {high order first} then
        for i := 1 to (sharedPtr^.ptrsize - hostintsize) * hostfileunits do
          putcbyte (0);

      for i := 1 to hostintsize * hostfileunits do
        putcbyte(kludge.b[i]);

      if hostintlowbytefirst <> reversebytes {low order first} then
        for i := 1 to (sharedPtr^.ptrsize - hostintsize) * hostfileunits do
          putcbyte (0);
      end
    else {pointer size = integer size}
      for i := 1 to sharedPtr^.ptrsize * hostfileunits do putcbyte(kludge.b[i]);
  end;
  {>>>}

  {<<<}
  procedure flushpackbuffer;
  { Flush anything in the packed constant buffer and update baseloc and
  curloc to reflect the new location.
  }
  var
    bytes: addressrange; {how many bytes it takes to hold the value}

  begin
    if curloc > baseloc then
      begin
      bytes := (curloc - baseloc + bitsperunit - 1) div bitsperunit;
      if packinghightolow then alignpartialint(pbuf1, bytes);
      putcint(pbuf1, bytes);
      baseloc := ((baseloc + bitsperunit - 1) div bitsperunit + bytes) *
                 bitsperunit;
      curloc := baseloc;
      pbuf1 := 0;
      end;
  end;
  {>>>}
  {<<<}
  procedure flushbuffer;
  { Flush any partial value kept as an intvalue thus far. }

  var
    i: 1..cbufsize; {induction var}

  begin
    with value1.cvalue do
      begin
      if representation = ints then
        begin
        if reversebytes then
          reversestructure(cbuf, hostintsize * hostfileunits);
        if hostintlowbytefirst = reversebytes {high order first} then
          alignpartialint(cbuf.i, cbytes);
        intvalue := cbuf.i;
        negated := false; { PJS force init }
        end
      else if emitflag then
        begin
        for i := 1 to cbytes do
          putbyte (cbuf.b[i]);
        cbytes := 0;
        end;
      end;
  end;
  {>>>}
  {<<<}
  procedure putvalue (vloc: addressrange; {loc to put value}
                      eltstring: boolean; {target elt is a string}
                      whichbuf: boolean; {which string buffer}
                      packing: boolean; {this is being packed in}
                      eltsize: addressrange; {size of constant element}
                      var value1: operand {value to place} );
  { Put a constant value in "value" into the current structured constant.
    There is an assumption that only items capable of representation as integers will actually be packed.
    The packing is done by the routine "packedstore".
    Strings require some extra work as a length byte must prefix the actual data,
    as the result might need padding, and it might be a character needing conversion to a string!
  }
  var
    full: boolean; {a packed word is full}
    start, finish: addressrange; {start and end of constant in file}
    bytes: addressrange; {bytes already used in the packing buffer}

    {<<<}
    procedure putpackedvalue (data: integer; {bits to put}
                             dataloc: addressrange; {where to put bits}
                             datasize: addressrange {number of bits to put} );
    { Put one packed data item.  This code was inline until strings were
      added, as conversion of a character to a string requires both the
      length and data bytes to be emitted.
    }
    begin
      { If the value won't fit in this integer, then we need to flush the packing buffer }
      { *** ok *** }
      if dataloc + datasize > baseloc + bitsperunit * hostintsize then
        begin
        bytes := (dataloc - baseloc + bitsperunit - 1) div bitsperunit;
        if packinghightolow then
          alignpartialint (pbuf1, bytes);
        putcint (pbuf1, bytes);
        baseloc := ((dataloc + bitsperunit - 1) div bitsperunit) * bitsperunit;
        curloc := baseloc;
        pbuf1 := 0;
        end;

      packedstore (dataloc, datasize, baseloc, data, pbuf1, pbuf2, full);

      if full then
        begin {we write out the lower buffer}
        putcint (pbuf1, hostintsize);
        pbuf1 := pbuf2;
        pbuf2 := 0;
        baseloc := baseloc + hostintsize * bitsperunit;
        end;
      curloc := dataloc + datasize;
    end;
    {>>>}

  begin
    if packing and (value1.cvalue.representation = ints) then
      begin
      if eltstring then
        begin
        putpackedvalue (1, vloc, bitsperunit); {length of string = 1 char}
        putpackedvalue (value1.cvalue.intvalue, vloc + bitsperunit, eltsize - bitsperunit);
        end
      else
        putpackedvalue(value1.cvalue.intvalue, vloc, eltsize);
      end

    else { not packing or (representation <> ints) }
      begin
      if packing then
        begin
        bytes := (vloc - baseloc + bitsperunit - 1) div bitsperunit;
        if packinghightolow then
          alignpartialint (pbuf1, bytes);
        putcint (pbuf1, bytes);
        pbuf1 := 0;
        end
      else
        putcint (0, vloc - curloc);

      with value1.cvalue do
        case representation of
          {<<<}
          ints:
            if eltstring then
              begin {convert char to string w/length byte then pad}
              putcbyte (1);
              putcbyte (value1.cvalue.intvalue);
              if packing then
                putcint (0, eltsize div bitsperunit - 2)
              else
                putcint (0, eltsize - 2);
              end
            else
              putcint (intvalue, eltsize);
          {>>>}
          reals:
            putcreal (realvalue.realbuffer, sharedPtr^.targetrealsize);
          doubles:
            putcreal (realvalue.realbuffer, doublesize);
          ptrs:
            putcptr (ptrvalue);
          otherwise
            {<<<}
            begin
            if packing then
              bytes := eltsize div bitsperunit
            else
              bytes := eltsize;

            if (pos < 0) then {has not been dumped to file at all, yet}
              begin
              flushbuffer;
              dumpstr (bytes, whichbuf, eltstring);
              end

            else
              begin
              start := pos;
              if start >= sharedPtr^.stringfilecount then
                start := start + (sharedPtr^.stringtablelimit - sharedPtr^.stringfilecount);
              finish := start + len;

              if eltstring then
                putcbyte (len);
              while start < finish do
                begin
                seekstringfile (start);
                putcbyte (sharedPtr^.stringblkptr^[sharedPtr^.nextstringfile]);
                start := start + 1;
                end;

              seekstringfile (sharedPtr^.stringfilecount);

              if eltstring then
                len := len + 1; {a kludge, the length byte}

              { This fixes structured constant bug tr2158.  Odd-byte length
                structures actually allocated the next greater even numbered
                byte worth of space and were not padded correctly.  Putvalue
                only emitted the actual data bytes but moved the current
                location counter to the allocated boundary.  This change
                causes Putvalue to correctly emit the actual data bytes, and
                eltsize-len (i.e. allocated length - data length) bytes of pad. }
              if bytes < len then
                if eltstring then
                  warn (stringoverflowerr)
                else
                  warn (compilerwritererr);

              putcint (0, bytes - len);
              end;
            end;
            {>>>}
          end;

      curloc := vloc + eltsize;
      baseloc := curloc;
      end;
  end;
  {>>>}
  {<<<}
  procedure initcdump (var value1: operand; {value to initialize}
                       consttypeindx: tableIndex; {index for this element type}
                       consttype: entryptr {type of this element} );
  { Initialize the value of the constant and set up the constant buffer for generating the value. }

  var
    t: addressrange; {aligned value of consttablelimit}

  begin
    cbytes := 0;
    cbuf.i := 0;
    pbuf1 := 0;
    pbuf2 := 0;
    curloc := 0;
    baseloc := 0;

    with value1 do
      begin
      typeindex := consttypeindx;
      oprndlen := sizeof(consttype, false);
      operandkind := constoperand;
      end;

    t := forcealign (sharedPtr^.stringfilecount, alignmentof(consttype, false) * hostfileunits, false);
    putcint (0, t - sharedPtr^.stringfilecount);

    curloc := 0;
    baseloc := 0;

    value1.extended := false;
    value1.cvalue.representation := consttype^.typ;
    value1.cvalue.stringconstflag := false;
    value1.cvalue.pos := t;
    value1.cvalue.len := value1.oprndlen * hostfileunits;
    if value1.cvalue.representation = ints then
      value1.cvalue.negated := false; {PJS force init }
  end;
  {>>>}
  {<<<}
  procedure finishdump(var value1: operand; {value being finished off}
                       packing: boolean {this is a packed value} );
  { After the entire constant has been parsed, this forces final output,
    if necessary, and finishes off the resulting operand
  }
  begin
    if value1.oprndlen > curloc then
      if packing then
        begin
        flushpackbuffer;
        if value1.oprndlen > curloc then
          putcint(0, (value1.oprndlen - curloc) div bitsperunit);
        end
      else
        putcint(0, value1.oprndlen - curloc);

    flushbuffer;
  end;
  {>>>}
  {<<<}
  procedure innercstruct (eltloc: addressrange; {rel loc for this element}
                         form: tableIndex; {form of this constant}
                         outerpacked: boolean {outer structure was packed} );
  { Given a structured value, determine the type and parse it }

  var
    f: entryptr; {for access to form}
    packedflag: boolean; {form is packed}

    {<<<}
    procedure constvalue(eltloc: addressrange; {rel loc for this element}
                         elttype: tableIndex; {desired value type}
                         eltstring: boolean; {current element is a string}
                         packing: boolean; {set if packing this element}
                         var value1: operand; {result, if not written}
                         var written: boolean {value already written} );
    { Parse a constant value.  If the value is in turn a structured constant,
      it may be written to the string file as it is scanned.  Otherwise,
      its value is placed in "value1", and must be written.
    }
      var
        tindex: tableIndex; {type name index}
        p: entryptr; {access to type name block}
        elp: entryptr; {for access to elttype}
        i: 1..cbufsize; {induction var for conversion}

      {<<<}
      procedure getconstant(follow: tokenset; {legal following symbols}
                            elttype: tableIndex; {desired value type}
                            var value1: operand {result} );
      { Parse a simple constant and make sure it's in range for the field
        it's being assigned to.
      }

        var
          elp: entryptr; {for access to elttype}
          unsvalue: unsignedint; {for unsigned comparisons}


        begin {getconstant}
          constant(follow, false, value1);
          with value1, cvalue do
            if (representation = ints) and (elttype <> noneindex) and
               compatible(typeindex, elttype) then
              begin
              elp := ref(bigtable[elttype]);
              if extended and not elp^.extendedrange or (intvalue < 0) and
                 negated and elp^.extendedrange then
                warnbefore(badconsterr)
              else if elp^.extendedrange then
                begin
                unsvalue := intvalue;
                if (unsvalue < lower(elp)) or
                   (unsvalue > upper(elp)) then
                  warnbefore(badconsterr);
                end
              else if (intvalue < lower(elp)) or
                      (intvalue > upper(elp)) then
                warnbefore(badconsterr);
              end;
        end {getconstant} ;
      {>>>}

    begin
      written := false;
      if token = ident then
        begin
        search (tindex);
        p := ref(bigtable[tindex]);
        with p^ do
          if namekind = typename then
            begin
            if not compatible (typeindex, elttype) then
              warn (typesincomp);
            gettoken;
            innercstruct (eltloc, typeindex, packing);
            written := true;
            end
          else
            getconstant (follow, elttype, value1);
        end

      else if token = lpar then
        begin
        elp := ref(bigtable[elttype]);
        if not (elp^.typ in [arrays, fields, none]) then
          begin
          warn (badconsterr);
          value1.typeindex := noneindex;
          value1.cvalue.representation := ints;
          end;
        innercstruct (eltloc, elttype, packing);
        written := true;
        end
      else
        getconstant (follow, elttype, value1);

      if not written then
        begin
        elp := ref(bigtable[value1.typeindex]);
        if not (eltstring and ((elp^.typ = chars) or (elp^.typ = arrays) and (elp^.stringtype)) or
           compatible (elttype, value1.typeindex)) then
          warnbefore (typesincomp);
        end;
    end;
    {>>>}
    {<<<}
    procedure constelement (eltloc: addressrange; {loc to for this element}
                            elttype: tableIndex; {type desired}
                            eltsize: addressrange; {size of the element}
                            eltstring: boolean; {current element is a string}
                            packing: boolean {this is a packed field} );
    { Read and store a constant element.  The result is written to the string
      file buffer.
    }
      var
        temp: operand; {holds a value}
        written: boolean; {value already written}
        whichbuf: boolean; {which string buf contains thistoken's string if
                            any}

      begin {constelement}
        whichbuf := sharedPtr^.curstringbuf;
        constvalue (eltloc, elttype, eltstring, packing, temp, written);
        if not written and (temp.typeindex <> noneindex) then
          putvalue (eltloc, eltstring, whichbuf, packing, eltsize, temp);
      end {constelement} ;
    {>>>}
    {<<<}
    procedure constarray (eltloc: addressrange; {rel loc for this element}
                         form: tableIndex {form for this array} );
    { Syntactic routine to parse a constant array.
      Each element is identical, and the number must match.
    }
      var
        eltcount: addressrange; {elements defined so far}
        eltsinarray: addressrange; {total elements in array}
        elttype: tableIndex; {element type}
        eltstring: boolean; {element is a string}
        packing: boolean; {set if packed array}
        eltsize: addressrange; {size of each element}
        f: entryptr; {for access to type data}


      begin {constarray}
        eltcount := 1;
        f := ref(bigtable[form]);
        elttype := f^.elementtype;
        packing := f^.packedflag;
        eltsize := f^.elementsize;
        eltsinarray := f^.arraymembers;
        f := ref(bigtable[elttype]);
        eltstring := f^.typ = strings;
        constelement(eltloc, elttype, eltsize, eltstring, packing);
        while token in [lpar, comma] + begconstset do
          begin
          verifytoken(comma, nocommaerr);
          eltloc := eltloc + eltsize;
          if eltcount = eltsinarray then warnbefore(badconsterr);
          eltcount := eltcount + 1;
          constelement(eltloc, elttype, eltsize, eltstring, packing);
          end;

        if eltcount < eltsinarray then warnbefore(badconsterr);
      end {constarray} ;
    {>>>}
    {<<<}
    procedure constrecord (eltloc: addressrange; {start of this element}
                          form: tableIndex {form for this record} );
    { Syntactic routine to parse a constant record }

    var
      currentfield: tableIndex; {name entry for this field}
      finished: boolean; {we used all of the fields we have}

      {<<<}
      procedure constfield;
      { Find the next field in this record and get a value for it. }

      var
        found: boolean; {field was found}
        p: entryptr; {access to field names}
        elttype: tableIndex; {form for a variant}
        temp: operand; {temp value for variant}
        written: boolean; {dummy argument to constvalue}
        tagoffset: addressrange; {offset for tag field, if any}
        localform: tableentry; {local copy of form entry}
        f, f1: entryptr; {access to form data}

      begin
        if finished then
          constelement (eltloc, noneindex, unitsize, false, false)
        else
          begin
          f := ref(bigtable[form]);
          localform := f^;
          found := false;

          p := nil;
          while (currentfield < localform.lastfield) and not found do
            begin
            currentfield := currentfield + 1;
            p := ref(bigtable[currentfield]);
            if not p^.form then
              found := p^.name = localform.fieldid;
            end;

          if found then
            begin
            f1 := ref(bigtable[p^.vartype]);
            constelement (p^.offset + eltloc, p^.vartype,
                          sizeof(f1, localform.packedflag), f1^.typ = strings, localform.packedflag)
            end
          else if localform.firstvariant = 0 then
            begin
            finished := true;
            warnbefore (badconsterr);
            constelement (curloc, noneindex, unitsize, false, false);
            end
          else
            begin
            tagoffset := 0;
            if localform.tagfield <> 0 then
              begin
              p := ref(bigtable[localform.tagfield]);
              elttype := p^.vartype;
              tagoffset := p^.offset;
              end
            else if localform.firstvariant <> 0 then
              begin
              f := ref(bigtable[localform.firstvariant]);
              if f^.firstlabel <> 0 then
                begin
                f := ref(bigtable[f^.firstlabel]);
                elttype := f^.varlabtype;
                end
              else
                elttype := noneindex;
              end
            else
              elttype := noneindex;

            constvalue (tagoffset + eltloc, elttype, false, localform.packedflag, temp, written);
            if localform.tagfield <> 0 then
              begin
              f := ref(bigtable[elttype]);
              putvalue (tagoffset + eltloc, false, false, localform.packedflag, sizeof(f, localform.packedflag), temp);
              end;

            searchvariants (form, temp);
            f := ref(bigtable[form]);
            currentfield := f^.firstfield - 1;
            end;
          end;
      end;
      {>>>}

    begin
      finished := false;
      f := ref(bigtable[form]);
      currentfield := f^.firstfield - 1;

      constfield;
      while token in [lpar, comma] + begconstset do
        begin
        verifytoken (comma, nocommaerr);
        constfield;
        end;

      f := ref(bigtable[form]);
      if (currentfield < f^.lastfield) or (f^.firstvariant <> 0) then
        warnbefore (badconsterr);
    end;
    {>>>}
    {<<<}
    procedure badconst;
    { Parse off a bad constant.  Just throws away the values. }

      begin {badconst}
        warnbefore(badconsterr);
        constelement(curloc, noneindex, unitsize, false, false);
        while token in [lpar, comma] + begconstset do
          begin
          verifytoken(comma, nocommaerr);
          constelement(curloc, noneindex, unitsize, false, false);
          end;
        value1.typeindex := noneindex;
        value1.cvalue.representation := ints;
      end {badconst} ;
    {>>>}

  begin
    if token = lpar then
      begin
      gettoken;
      f := ref(bigtable[form]);
      packedflag := f^.packedflag;
      if outerpacked and not packedflag then
        begin
        flushpackbuffer;
        eltloc := eltloc div bitsperunit;
        curloc := curloc div bitsperunit;
        end
      else if not outerpacked and packedflag then
        begin
        eltloc := eltloc * bitsperunit;
        curloc := curloc * bitsperunit;
        baseloc := curloc;
        end;
      if f^.typ in [strings, arrays] then constarray(eltloc, form)
      else if f^.typ = fields then constrecord(eltloc, form)
      else badconst;
      if not outerpacked and packedflag then
        begin
        flushpackbuffer;
        curloc := curloc div bitsperunit;
        end
      else if outerpacked and not packedflag then
        begin
        curloc := curloc * bitsperunit;
        baseloc := curloc;
        end;
      verifytoken(rpar, norparerr);
      end
    else
      begin
      warnbefore(typenotallowed);
      value1.typeindex := noneindex;
      value1.cvalue.representation := ints;
      end;
  end;
  {>>>}

begin
  gettoken;
  f := ref (bigtable[form]);
  packedflag := f^.packedflag;
  initcdump (value1, form, f);
  innercstruct (0, form, false);
  finishdump (value1, packedflag);
end;
{>>>}
{<<<}
procedure onevar (id: integer; varkind: nametype; var where: tableIndex; sharedvar: boolean);
{<<<}
{ Syntactic routine to parse a single component of a variable list,
  which can be part of a variable-list, parameter-list, or field-list.
  "Varkind" determines the kind of variable list being parsed.
  x-identifier = identifier [use/define 'name' | origin nnn]
  The identifier is entered into the symbol table at scope "id".
  If the identifier is a parameter, it is initialized on entry to the scope, so is marked as "modified".
}
{>>>}
var
  p: entryptr; {used to access name entry}
  assignedaddress: operand; {holds ORIGIN value or external name, if any}
  varblock: 0..maxvarptrs; {for calculating block number of USE/DEFINE var}
  alias_needed: boolean; {true if string follows USE/DEFINE var}
  vartab_state: (new_entry, reuse_entry, no_entry); {used with multidef switch to handle mutiple definitions}
  entry: integer; {Current vartableentry index}

begin
  {The following actually consumes the token}
  enterident(id, where, undefname);
  p := ref(bigtable[where]);
  with p^ do
    begin
    if namekind <> undefname then programdecl := false;
    namekind := varkind;
    lastinsection := false;
    univparam := false;
    vartab_state := new_entry;

    if sharedvar and (token in [definesym, usesym, originsym]) then
      warn(badsharedvar);

    if sharedvar or (token in [definesym, usesym]) then
      begin
      if sharedvar then
        begin
        if sharedPtr^.switcheverplus[multidef] and (varalloc in [definealloc, usealloc]) then
          warn(badmultidef);

        varalloc := sharedalloc;
        {point to the identifer text in the stringtable}
        assignedaddress.cvalue.pos := charindex;
        assignedaddress.cvalue.len := charlen;
        alias_needed := false;
        end
      else {use/define}
        begin
        if (varkind <> varname) or (level <> 1) then
          warn(badusedefinevar);
        if token = usesym then
          begin
          if sharedPtr^.switcheverplus[multidef] and
             (varalloc in [usealloc, definealloc]) then
            vartab_state := no_entry
          else varalloc := usealloc;
          end
        else
          begin
          if sharedPtr^.switcheverplus[multidef] then
            begin
            if varalloc = definealloc then
              warn(duplicateident); { Don't allow multiple 'define's }
            if not (varalloc in [normalalloc, usealloc]) then
              warn(badmultidef);
            if varalloc = usealloc then vartab_state := reuse_entry;
            end;
          varalloc := definealloc;
          end;
        gettoken;
        if token in begconstset then
          begin
          constant(begtypset + [colon, comma], true, assignedaddress);
          alias_needed := true;

          with assignedaddress, cvalue do
            if typeindex = chartypeindex then
              begin
              putbyte (intvalue);
              pos := sharedPtr^.stringfilecount - 1;
              len := 1;
              end
            else if (representation <> arrays) or not stringconstflag then
              warnbefore(nostringerr);
          end
        else
          begin { point to the identifer text in the stringtable }
          alias_needed := false;
          assignedaddress.cvalue.pos := charindex;
          assignedaddress.cvalue.len := charlen;
          end;
        end; {use/define}

      if vartab_state <> no_entry then
        begin
        { If multidef is true, then allow a define to overlay the vartable
          entry for a use, but ignore a use after a define }
        if vartab_state = reuse_entry then
          entry := sparelink
        else
          begin
          sharedPtr^.lastvartableentry := sharedPtr^.lastvartableentry + 1;
          entry := sharedPtr^.lastvartableentry;
          end;

        varblock := entry div (maxvarentries + 1) + 1;

        if varblock > sharedPtr^.lastvartableptr then
          begin
          if varblock > maxvarptrs then
            analysFatal (toomanyextvars)
          else
            begin
            new (sharedPtr^.vartable[varblock]);
            end;
          sharedPtr^.lastvartableptr := varblock;
          end;

        sparelink := entry;

        with sharedPtr^.vartable[varblock]^[entry mod (maxvarentries + 1)] do
          begin
          extvaralloc := varalloc;
          initialized := false; {never true for Pascal}
          referenced := false;
          aliased := alias_needed;
          charindex := assignedaddress.cvalue.pos;
          charlen := min(maxprocnamelen, assignedaddress.cvalue.len);
          size := 0;
          offset := 0;
          end;
        end; {vartab_state <> no_entry}
      end
    else if token = originsym then
      begin
      gettoken;
      constant(begtypset + [colon, comma], true, assignedaddress);
      if (varkind = varname) and
         (assignedaddress.cvalue.representation = ints) and
         not assignedaddress.cvalue.negated then
        begin
        {**note: following assignment also converts int to unsigned**}
        if sharedPtr^.switcheverplus[multidef] and
           (varalloc in [definealloc, usealloc]) then warn(badmultidef);
        varalloc := absolute;
        offset := assignedaddress.cvalue.intvalue;
        end
      else
        warnbefore(badorigin);
      end
    else if (varkind = varname) and (level = 1) and
            (sharedPtr^.switchcounters[own] > 0) then
      begin
      if sharedPtr^.switcheverplus[multidef] and
         (varalloc in [definealloc, usealloc]) then warn(badmultidef);
      varalloc := ownalloc;
      end
    else { It's none of the above -- must be normalalloc }
      begin
      if sharedPtr^.switcheverplus[multidef] and
         (varalloc in [definealloc, usealloc]) then warn(badmultidef);
      varalloc := normalalloc;
      end;

    registercandidate := varalloc = normalalloc; { until proven otherwise }
    nextparamlink := where;
    nestedmod := false;
    varianttag := false;
    knownvalid := (namekind in
                  [procparam, funcparam, param, varparam, boundid]);
    modified := (varalloc = absolute) or knownvalid or sharedPtr^.switcheverplus[test];
    parammodified := false;
    end;
end;
{>>>}
{<<<}
procedure alloconevar (t: tableIndex; f: tableIndex; varkind: nametype; var size: addressrange;
                       a: alignmentrange; typelen: addressrange; refparam: boolean; packedresult: boolean );
{<<<}
{ Allocate one variable in a variable or field list.  "Size" is the current
  size of the dataspace in which the variable is being allocated, and is
  updated after the allocation.
  "Size" and "typelen" are in bits if this is a packed allocation, otherwise
  in addressing units.
  This procedure defines the packed allocation strategy, which is to allocate
  bitwise, but not to allow fields to cross word boundaries.
}
{>>>}
var
  p: entryptr; {used for access to name entry}
  newoffset: addressrange; {offset of new variable}
  unusedspace: boolean; {we skipped over some space in packed field}
  overflowed: boolean; {true if data space is too large}

begin
  p := ref(bigtable[t]);
  p^.namekind := varkind;

  if sharedPtr^.switcheverplus[multidef] and (p^.namekind = varname) and
     (p^.varalloc in [usealloc, definealloc]) and
     not identical(p^.vartype, f) then warn(badmultidef);

  p^.vartype := f;

  if refparam then
     begin
     if sharedPtr^.switcheverplus[multidef] and
        (p^.varalloc in [definealloc, usealloc]) then warn(badmultidef);
     p^.varalloc := pointeralloc;
     end;

  if p^.varalloc = absolute then
    begin
    end
  else if p^.varalloc in [usealloc, definealloc, sharedalloc] then
    begin
    with sharedPtr^.vartable[p^.sparelink div (maxvarentries + 1) + 1]^
                            [p^.sparelink mod (maxvarentries + 1)] do
      begin
      if (p^.varalloc = definealloc) and (size = 0) then
        {we checked size to avoid multiple allocation with multidef}
        begin
        alloc (a, typelen, sharedPtr^.definesize, newoffset, overflowed);
        offset := newoffset;
        end;
      size := typelen;
      end;

    p^.offset := 0;
    end
  else
    begin
    if p^.varalloc = ownalloc then
      if packedresult then
        allocpacked(a, typelen, sharedPtr^.ownsize, newoffset, overflowed, unusedspace)
      else
        alloc(a, typelen, sharedPtr^.ownsize, newoffset, overflowed)
    else if packedresult then
      allocpacked(a, typelen, size, newoffset, overflowed, unusedspace)
    else
      alloc(a, typelen, size, newoffset, overflowed);

    p^.offset := newoffset;
    if overflowed then
      if varkind = fieldname then warnbefore(bigrecorderr)
      else warnbefore(bigblockerr);
    end;
  p^.length := typelen;
end;
{>>>}
{<<<}
procedure variablelist (follow: tokenset; f1: tokenset; id: integer; dbgscope: p_symbolindex;
                        var size: addressrange; var align: alignmentrange;
                        varkind: nametype; notrecord: boolean; packedresult: boolean; sharedvar: boolean);
{<<<}
{ Syntactic routine to parse a group of variable lists of any flavor.
  Productions:
  x-list = [* ";" *] [* identifier [* "," identifier *]
        ":" type [ ";" ] *]  .

  Leading semicolons are accepted only within a record, as determined
  by the parameter "notrecord".

  This routine is complicated by being called in several contexts and
  with several purposes.  In particular, the routine is parsing an
  entire group of variable declarations, and attempts do as much error
  recovery locally as possible.  On the other hand, in some contexts
  the routine should stop after each declaration.  This is controlled
  by the internal set "startset" and the parameter "f1".

  "Startset" contains essentially all of the tokens which might appear
  in a variable list.  In particular, it contains all of the punctuation
  and all of the type beginning symbols.  "F1" contains all of the
  tokens on which the scan should stop after the first variable list
  is parsed.  For instance, in a field list, "f1" contains all of
  the tokens which might begin the next fields. The details
  of usage can be seen by examining the calling locations.

  All identifiers found are entered in the scope specified by "id", and
  given the kind "varkind".  On entry to the procedure, "size" is the
  current size of the data space, and "align" is the minimum alignment
  requirement for the variables.  Space allocated is reflected
  by "size", and the maximum alignment is accumulated and returned in
  "align".

  The case where a variable may already exist (due to previous naming
  as a program parameter) complicates things.  The variable list is
  linked upon "nextparamlink", in a special way (the next item in
  the list equals "nextparamlink + 1").  It is done this way due to
  the fact that "nextparamlink" is being overloaded, as it is really
  intended to link parameter lists, skipping over nested procedure
  parameters.
}
{>>>}
{<<<}
var
  startset: tokenset; {All tokens to be found in a varlist}
  first: tableIndex; {first ident in a list}
  last: tableIndex; {last ident in a list}
  t: tableIndex; {induction var for allocation}
  f: tableIndex; {type of a variable list}
  tptr, fptr: entryptr; {for access to the pointer}
  a: alignmentrange; {alignment for one varlist}
  typelen: addressrange; {length of one varlist element}
  varcount: integer; {number of variables in list}
  startpos: integer; {position of first var in source file}
  startline: integer; {line number of first var}
{>>>}

begin
  align := unitsize;
  startset := [uparrow..stringsym, nilsym, ident, plus, minus, lpar, intconst..stringconst, comma, colon, semicolon];

  {skip semicolons if processing field list}
  if notrecord then
    verify ([ident], follow, novarerr)
  else while token = semicolon do
    gettoken;

  while token in startset do
    begin {parse one variable list, making sure that it terminates on one of the tokens in f1}
    startset := startset - f1;
    startpos := thistoken.filepos;
    startline := thistoken.line;

    onevar (id, varkind, first, sharedvar);
    varcount := 1;
    last := first;
    while token in [ident, comma] do
      begin
      verifytoken (comma, nocommaerr);
      f := last;
      onevar (id, varkind, last, sharedvar);
      varcount := varcount + 1;
      if last - 1 <> f then
        begin
        fptr := ref(bigtable[f]);
        fptr^.nextparamlink := last - 1;
        end;
      end;

    f := noneindex;
    verifytoken (colon, nocolonerr);
    gettyp (follow + startset, f);

    fptr := ref(bigtable[f]);
    if notrecord and fptr^.containsfile then
      sharedPtr^.proctable[display[level].blockref].opensfile := true;

    t := first;
    repeat
      getallocdata (fptr, varkind, packedresult, size, typelen, a, align);
      alloconevar (t, f, varkind, size, a, typelen, false, packedresult);
      tptr := ref (bigtable[t]);
      t := tptr^.nextparamlink + 1;
      fptr := ref (bigtable[f]);
      varcount := varcount - 1;
    until varcount = 0;

    { must end on semicolon or in f1 for no error }
    if token = semicolon then
      gettoken
    else
      verify (f1, follow + begtypset + [comma, colon, semicolon], nosemiheaderr);
    end;
end;
{>>>}
{<<<}
procedure gettyp (follow : tokenset; var resulttype: tableIndex);
{ Syntactic routine to parse a type }

var
  packflag: boolean; { set if "packed" found}
  resulp: entryptr;  { pointer for access to resulttype}

  {<<<}
  procedure ptrtyp;
  { Syntactic routine to parse a pointer type.
    Production:
    pointer-type = "^" type-identifier  .
    This is complicated by the fact that pointer types can refer to types
    which are not yet defined.  In this case, the referenced type is entered
    as an undefined type name, and a reference to it is kept in the
    undeftable for checking.
    Since travrs needs to be able to group pointers according to type,
    each pointer type is assigned a unique key (generated from lastfilekey)
    which serves to identify it to travrs.  This key is not used further
    in analys.
  }

    var
      t: tableIndex; { index for type identifier }
      p: entryptr; {used for name access to t}
      newtype: tableIndex; {points to type of type name if found}


    begin {ptrtyp}
      if packflag then warn(cantpack);
      gettoken;
      enterform (ptrs, resulttype, resulp);
      with resulp^ do
        begin
        ptrkey := lastfilekey;
        lastfilekey := lastfilekey - 1;
        size := sharedPtr^.ptrsize;
        align := ptralign;
        ptrtypename := 0;
        if token = ident then
          begin
          probing := true;
          search (t);
          probing := false;
          p := ref(bigtable[t]);
          if (t = 0) or
             not (p^.namekind in [undeftypename, noname, typename]) then
            begin
            newtype := noneindex;
            if t <> 0 then
              if (p^.lastoccurrence = display[level].scopeid) then
                warn(notypenameerr);
            end
          else newtype := p^.typeindex;
          if (t = 0) or (lev < level) and
             (p^.lastoccurrence < display[level].scopeid) then
            begin
            enterlocalident(t, noname);
            enterundef(t);
            p := ref(bigtable[t]);
            p^.namekind := undeftypename;
            p^.typeindex := newtype;
            end
          else
            gettoken;

          resulp = ref(bigtable[resulttype]);
          resulp^.ptrtypename := t
          end
        else
          warnbetween(novarerr);
        end;
    end {ptrtyp} ;
  {>>>}
  {<<<}
  procedure arraytyp;
  { Syntactic routine to parse an array type }

    {<<<}
    procedure arraywork (var resulttype: tableIndex);
    { Syntactic routine to parse most of an array declaration.  The productions
      used are given above for "arraytyp".  As successive index types are found,
      this procedure is invoked recursively to treat it as:
      array [ index-type ] of array [index-type] of ...
    }
    var
      es: addressrange;           { tentative array element size}
      elttype: tableIndex;        { type pointer for element}
      resulp: entryptr;           { for access to resulttype}
      f: entryptr;                { used to get access to type records}
      newindextype: tableIndex;   { index type}
      t: 0..maxusint;             { temp for unsigned arithmetic}
      indexmembers: addressrange; { members of the index type}
      isstring: boolean;          { set if it is a string}

    begin
      gettyp (follow + [ofsym, comma, rpar, rbrack] + begsimplset, newindextype);
      f := ref(bigtable[newindextype]);

      {Writeln ('arraywork:', f^.typ, ' newindextype:',newindextype);
      }
      if not (f^.typ in [none, chars, bools, scalars, subranges]) then
        begin
        warnbefore (badindex);
        newindextype := noneindex;
        end;

      t := upper(f) - lower(f) + 1;
      if t > maxaddr then
        warnbefore (toomanyelements);

      indexmembers := t;
      isstring := (lower(f) = 1) and
                  ((upper(f) > 1) or (sharedPtr^.switchcounters[standard] <= 0));
      if isstring then
        isstring := getform(f) in [ints, none];

      if token in [comma, ident, nilsym, lpar, plus, minus, intconst..stringconst] then
        begin
        verifytoken (comma, nocommaerr);
        arraywork (elttype);
        end
      else
        begin
        if token = rpar then
          begin
          warn (norbrackerr);
          gettoken
          end
        else
          verifytoken (rbrack, norbrackerr);

        verifytoken (ofsym, nooferr);
        gettyp (follow, elttype)
        end;

      f := ref(bigtable[elttype]);
      if isstring then
        isstring := f^.typ in [chars, none];

      enterform (arrays, resulttype, resulp);
      with resulp^ do
        begin
        indextype := newindextype;
        packedflag := packflag;
        elementtype := elttype;
        arraymembers := indexmembers;

        es := arraysizeof(f, packflag);
        if es = 0 then
          es := 1; {undefined kind}

        { determine if array size less than one word }
        bitaddress := packflag and (es < (packingunit * bitsperunit)) and
                      (indexmembers < (packingunit * bitsperunit) div es);

        if not bitaddress and packflag and
           (es < packingunit * bitsperunit) then
          { packed array > word but element < word }
          if es < bitsperunit then
            size := (indexmembers + (bitsperunit div es) - 1) div (bitsperunit div es)
          else
            size := ((es + bitsperunit - 1) div bitsperunit) * indexmembers
        else
          begin
          es := arraysizeof(f, bitaddress);
          if not bitaddress and (es > 1) and (maxaddr div es < indexmembers) then
            begin
            indexmembers := 1; {prevent overflow}
            warnbefore (bigarrayerr);
            end;
          size := indexmembers * es;
          if packflag and not bitaddress then
            es := es * bitsperunit;
          end;
        elementsize := es;

        if bitaddress then
          align := es
        else if packflag and (es <= bitsperunit) then
          align := unitsize
        else
          align := alignmentof(f, false);
        containsfile := f^.containsfile;
        stringtype := isstring and packflag;
        end;
    end;
    {>>>}

  begin
    gettoken;
    if token = lpar then
      begin
      warn (nolbrackerr);
      gettoken
      end
    else
      verifytoken (lbrack, nolbrackerr);

    arraywork (resulttype);
  end;
  {>>>}
  {<<<}
  procedure filetyp;
  { Syntactic routine to parse a file type }

  var
    newfilebasetype: tableIndex; {pointer to type of file}
    f: entryptr; {access to newfilebasetype}

  begin
    gettoken;
    verifytoken (ofsym, nooferr);
    gettyp (follow, newfilebasetype);

    f := ref(bigtable[newfilebasetype]);
    if f^.containsfile then
      warnbefore(nofilefile);

    enterform (files, resulttype, resulp);
    with resulp^ do
      begin
      filebasetype := newfilebasetype;
      packedflag := packflag;
      bitaddress := false;
      containsfile := true; { You had better believe it!!! }
      size := sharedPtr^.ptrsize;
      align := ptralign;
      filekey := lastfilekey;
      lastfilekey := lastfilekey - 1;
      end
  end;
  {>>>}
  {<<<}
  procedure settyp;
  { Syntactic routine to parse a set type.
    Productions:
    set-type = "set" "of" base-type  .
    base-type = ordinal-type.
    Packed sets are allocated the exact number of bits needed, while
    ordinary sets are allocated an integral number of addressing units.
    If a set is not packed, and the size is greater than a word, the
    set is aligned on a word boundary.
  }
  var
    m: integer; {number of members of the set}
    newbasetype: tableIndex; {type of set base}
    f: entryptr; {access to newbasetype}

  begin {settyp}
    gettoken;
    verifytoken(ofsym, nooferr);
    gettyp(follow, newbasetype);

    f := ref(bigtable[newbasetype]);
    if not (f^.typ in [none, scalars, bools, chars, subranges]) then
      warnbefore (badsetbase)
    else if (lower(f) < 0) or (upper(f) > maxsetord) then
      warnbefore (bigsetbase);

    stripsubrange (newbasetype);
    f := ref(bigtable[newbasetype]);
    if f^.typ = ints then
      m := maxsetord + 1
    else
      m := upper(f) + 1;

    enterform (sets, resulttype, resulp);
    with resulp^ do
      begin
      constructedset := false;
      basetype := newbasetype;
      packedflag := packflag;
      bitaddress := packflag;
      if packedflag then
        begin
        if (m > bitsperunit) then
          size := forcealign(m, 1 { a byte }, true)
        else size := roundpackedsize(m, true);
        if size > bitsperunit then align := setalign * bitsperunit
        else align := 1;
        end
      else
        begin
        size := (m + bitsperunit - 1) div bitsperunit;
        if size = unitsize then align := unitsize
        else align := setalign;
        size := forcealign(size, align, false);
        end;
      end;
  end {settyp} ;
  {>>>}
  {<<<}
  procedure stringtyp;
  { Process string type, a non-standard and (many of us think) ugly extension
    enthusiastically embraced by the unwashed masses of PC-land.
    Productions:
    string-type = "string" "[" constant "]".
  }

  var
    value: operand; {value returned by constant}
    t: tableIndex; {temp index for entering string type}
    t1: entryptr; {Temp ptr for entering string type}

  begin {stringtyp}
    gettoken;
    if token = lpar then
      begin
      warn(nolbrackerr);
      gettoken
      end
    else verifytoken(lbrack, nolbrackerr);
    constant(follow + [rbrack], true, value);
    if (value.typeindex <> intindex) or (value.cvalue.intvalue <= 0) or
       (value.cvalue.intvalue > 255) then
      begin
      warnbefore(badstringindex);
      resulttype := noneindex;
      end
    else
      begin
      enterform (subranges, t, t1);
      with t1^ do
        begin
        size := sharedPtr^.targetintsize;
        align := intalign;
        parenttype := intindex;
        parentform := ints;
        lowerord := 0;
        upperord := value.cvalue.intvalue;
        end;

      enterform (strings, resulttype, resulp);
      with resulp^ do
        begin
        packedflag := true;
        bitaddress := true;
        containsfile := false;
        elementtype := chartypeindex;
        stringtype := false;
        arraymembers := value.cvalue.intvalue + 1;
        indextype := t;
        size := arraymembers div (bitsperunit div stringeltsize);
        if arraymembers mod (bitsperunit div stringeltsize) <> 0 then
          size := size + 1;
        size := size * bitsperunit;
        elementsize := stringeltsize;
        align := bitsperunit;
        end;
      end;

    if token = rpar then
      begin
      warn(norbrackerr);
      gettoken
      end
    else
      verifytoken(rbrack, norbrackerr);
  end;
  {>>>}
  {<<<}
  procedure recordtyp;
  { Syntactic routine to parse a record type.
    Productions:
    record-type = "record" [ field-list [ ";" ] ] "end" .
    As far as the compiler is concerned, the record type simply
    defines a new scope, and all of the real work is done in the
    "fieldlist" routine.
    }

    var
      id: integer; {scope id for this record}
      dbgscope: p_symbolindex; {index of record's debug entry}
      p: entryptr;

    {<<<}
    procedure fieldlist(follow: tokenset; {legal following symbols}
                        tag: tableIndex; {tag field for this field, if any}
                        tagl: tableIndex; {list of label entries, if any}
                        lastv: tableIndex; {last variant parsed}
                        startsize: addressrange; {size at start of list}
                        var result: tableIndex {resulting type});
    { Syntactic routine to parse a fieldlist.
      Productions:
      field-list = fixed-part [ ";" variant-part ] | variant-part  .
      fixed-part = record-section [* ";" record-section *]  .
      record-section = identifier-list ":" type-denoter  .
      variant-part = "case" variant-selector "of" variant
            [* ";" variant *]  .
      variant-selector = [ identifier ":" ] type  .
      variant = case-constant-list ":" "(" [ fieldlist [ ";" ] ] ")" .
      case-constant-list = constant [* "," constant *]  .

      Each field-list is represented as a formentry with an index for the
      first and last field entries in the name table.  Each variant is
      treated as a separate fieldlist, and all variants for a fieldlist
      are linked through the "nextvariant" field in the formentry.  The
      "firstvariant" field is the start of this chain.  The labels for
      a particular variant are kept on a list rooted in "firstlabel", and
      a pointer to the tagfield is kept in "tagfield".

      Access to the fields is through the normal lookup using the "fieldid"
      as a scope id, or by sequential scan of the name entries when necessary,
      as for structured constants or writing debugger tables.  Note that
      not all name entries between "firstfield" and "lastfield" are field
      identifiers, only the ones with the proper scope id.

      "Tagl" is the list of case labels for this variant (or zero), and
      "lastv" is the last variant for linking (or zero).
    }
    var
      resulp: entryptr; {access to result}
      localresult: tableentry; {local copy of "result" to simplify code}
      a: alignmentrange; {temp value of alignment for tagfield}
      latestlabel: tableIndex; {last variant label parsed}
      latestvariant: tableIndex; {last variant parsed}
      tagcount: unsignedint; {number of tags defined}
      tagmembers: unsignedint; {number of elements in tag type}
      lowesttag, highesttag: integer; {for checking that all tags defined}
      casetype: tableIndex; {type of tagfield}
      caseptr: entryptr; {access to casetype}
      f1: tableIndex; {temp for fieldlist within variant}
      f1ptr: entryptr; {access to f1}
      t: tableIndex; {tag field name entry (0 for undiscriminated)}
      p: entryptr; {used for name access}
      oldany: boolean; {old value of anyfile}

    {<<<}
    procedure onelabel;
    { Syntactic routine to parse a single variant label (case-constant in
      terms of the latest draft standard).
      Production:
      case-constant = constant.
      All labels for a given variant are chained together and the chain is
      rooted in the formentry for that variant.
    }

      var
        t: tableIndex; {temp ptr of various uses}
        f: entryptr; {used for access to forms}
        labelval: operand; {constant label value}

      {<<<}
      procedure checklabs(header: tableIndex {start of a list of labels} );
      { Check all of the labels on a particular label list to see if there are any
        with the same value as the current label.
      }

        var
          f: entryptr; {used to access labels}


        begin {checklabs}
          while (header <> 0) do
            begin
            f := ref(bigtable[header]);
            with f^ do
              begin
              if (labelval.cvalue.intvalue = varlabvalue) then
                warnbefore(dupcaselabel);
              header := nextvarlab
              end;
            end;
        end {checklabs} ;
      {>>>}

      begin {onelabel}

        tagcount := tagcount + 1;
        constant(follow + [comma, colon, lpar, rpar], true, labelval);
        with labelval.cvalue do
          if (intvalue < lowesttag) or (intvalue > highesttag) then
            warnnonstandard(badcasetags);
        checklabs(latestlabel);

        t := latestvariant;
        while (t <> 0) do
          begin
          f := ref(bigtable[t]);
          with f^ do
            begin
            t := nextvariant;
            checklabs(firstlabel);
            end;
          end;

        enterform (variantlabs, t, f);
        with f^ do
          begin
          packedflag := packflag;
          bitaddress := packflag;
          nextvarlab := latestlabel;
          varlabtype := labelval.typeindex;
          varlabvalue := labelval.cvalue.intvalue;
          end;

        latestlabel := t;
        if (casetype <> noneindex) and (labelval.typeindex <> casetype) then
          warnbefore(badcaselab);

      end {onelabel} ;
    {>>>}

    begin {fieldlist}
      oldany := anyfile;
      anyfile := false;
      enterform (fields, result, resulp);
      localresult := resulp^;
      with localresult do
        begin
        packedflag := packflag;
        bitaddress := packflag;
        fieldid := id;
        {link onto variant list}
        nextvariant := lastv;
        firstlabel := tagl;
        firstvariant := 0;
        tagfield := 0;
        typ := fields;
        firstfield := tabletop + 1;
        variablelist(follow + [rpar, casesym, endsym], [rpar, endsym], id,
                     dbgscope, startsize, a, fieldname, false, packflag, false);
        lastfield := tabletop + 1;
        repeat
          lastfield := lastfield - 1;
          p := ref(bigtable[lastfield]);
        until (p^.name = fieldid) or (lastfield < firstfield);
        containsfile := anyfile;
        anyfile := oldany;

        {Now parse a variant-part}

        if token = casesym then
          begin
          gettoken;
          if (token = ident) and (tokenSharedPtr^.nexttoken.token = colon) then
            begin
            onevar(id, fieldname, t, false);
            p := ref(bigtable[t]);
            p^.varianttag := true;
            tagfield := t; { tagfield is in a packed record }
            gettoken;
            end;
          casetype := noneindex;
          if token in
             [uparrow..stringsym, nilsym, intconst..stringconst, plus,
             minus, lpar] then
            warnnonstandard(notypenameerr);
          gettyp(follow + [ofsym, endsym, ident, colon], casetype);
          if tagfield <> 0 then
            begin {allocate a tag field}
            caseptr := ref(bigtable[casetype]);
            a := max(a, alignmentof(caseptr, packflag));
            alloconevar(tagfield, casetype, fieldname, startsize,
                        alignmentof(caseptr, packflag), sizeof(caseptr,
                        packflag), false, packflag);
            end;

          caseptr := ref(bigtable[casetype]);
          lowesttag := lower(caseptr);
          highesttag := upper(caseptr);
          stripsubrange(casetype);
          if not (caseptr^.typ in
             [subranges, ints, chars, bools, scalars, none]) then
            warn(badcasetyp);
          verifytoken(ofsym, nooferr);
          latestvariant := 0;
          tagmembers := highesttag - lowesttag + 1;
          tagcount := 0;
          size := startsize;
          while token in
                [comma, colon, lpar, semicolon, plus, minus, ident, nilsym,
                intconst..stringconst] do
            begin {parse a single variant}
            if token in
               [plus, minus, ident, nilsym, intconst..stringconst, comma,
               colon, lpar] then
              begin
              latestlabel := 0;
              onelabel;
              while token in
                    [plus, minus, ident, nilsym, intconst..stringconst,
                    comma] do
                begin
                verifytoken(comma, nocommaerr);
                onelabel;
                end;
              verifytoken(colon, nocolonerr);
              verifytoken(lpar, nolparerr);
              fieldlist(follow + [comma, colon, rpar], tagfield,
                        latestlabel, latestvariant, size, f1);
              f1ptr := ref(bigtable[f1]);
              containsfile := containsfile or f1ptr^.containsfile;
              f1ptr^.packedflag := packflag;
              f1ptr^.bitaddress := packflag;
              if f1ptr^.size > startsize then startsize := f1ptr^.size;
              a := max(a, alignmentof(f1ptr, packflag));
              latestvariant := f1;
              verifytoken(rpar, norparerr);
              end;
            if token = semicolon then gettoken
            else
              verify([endsym, rpar], follow + [comma, colon],
                     nosemiheaderr);
            end;
          firstvariant := latestvariant;
          if tagmembers <> tagcount then warnnonstandard(badcasetags);
          end;
        size := roundpackedsize(startsize, packflag);
        align := a;
        end;
      anyfile := oldany;
      resulp := ref(bigtable[result]);
      resulp^ := localresult;
    end {fieldlist} ;
    {>>>}


    begin {recordtyp}
      gettoken;
      if (lastid >= totalscopes) or (lastscope >= totalscopes) then
        analysFatal (manyscopes); {the last value of totalscopes is not used.}
      lastid := lastid + 1;
      lastscope := lastscope + 1;
      id := lastid;

      { Create field entry now so ODB can create a hash table for the new scope. }
      fieldlist(follow + [endsym], 0, 0, 0, 0, resulttype);

      verifytoken(endsym, noenderr);
    end {recordtyp} ;
  {>>>}
  {<<<}
  procedure simpletyp;
  { Syntactic routine to parse a simple type.
    simple-type = type-identifier | subrange-type | enumerated-type  .
    subrange-type = constant ".." constant  .
    enumerated-type = "(" identifier [* "," identifier *] ")"  .
  }
    var
      lowervalue, uppervalue: operand; {ends of a subrange}
      extended: boolean; {range doesn't fit in host integer}
      t: tableIndex; {type identifier index}
      p: entryptr; {used for name access}
      f: entryptr; {used for access to form data}
      latestord: integer; {ord of last scalar parsed in enumerated type}
      subrange: boolean; {true if subrange being parsed}

    {<<<}
    procedure onescalar;
    { Syntactic routine to parse a single identifier in an enumeration type.
      Production:
      enumeration-constant = identifer  .
      The identifier is entered in the local scope (not the record scope, but
      the surrounding local scope).  Successive constants are assigned successive ordinal values. }

    var
      t: tableIndex; {index of enumeration constant id}

    begin
      enterlocalident (t, noname);

      p := ref(bigtable[t]);
      with p^, constvalue do
        begin
        namekind := constname;
        consttype := resulttype;
        representation := ints;
        intvalue := latestord;
        negated := false;
        end;
    end;
    {>>>}

  begin
    resulttype := noneindex;
    if packflag then
      warn(cantpack);

    if token = lpar then
      {<<<  enumeration type, parse constants}
      begin
      gettoken;
      enterform (scalars, resulttype, resulp);
      latestord := 0;
      onescalar;

      while token in [comma, ident] do
        begin
        latestord := latestord + 1;
        verifytoken (comma, nocommaerr);
        onescalar;
        end;

      resulp := ref(bigtable[resulttype]);
      with resulp^ do
        begin
        lastord := latestord;
        size := simplesize (lastord);
        align := min (scalaralign, size);
        end;

      verifytoken (rpar, norparerr);
      end
      {>>>}
    else { not enumerated }
      begin
      verify (begconstset, follow, badtypesyntax);
      subrange := token in begconstset;
      if token = ident then
        {<<<  ident}
        begin
        search (t);

        p := ref(bigtable[t]);
        if (t = 0) or (p^.namekind = undeftypename) and
           (p^.typeindex = noneindex) or (p^.namekind = noname) then
          begin {undefined, assume was meant to be type id.}
          warn (undefidenterr);
          subrange := false;
          gettoken;
          end

        else if p^.namekind in [undeftypename, typename] then
          begin
          resulttype := p^.typeindex;
          subrange := false;
          gettoken
          end
        end;
        {>>>}
      if subrange then
        {<<<  subrange}
        begin
        constant (follow + [dotdot], true, lowervalue);
        verifytoken (dotdot, nodotdoterr);

        constant (follow, true, uppervalue);
        extended := uppervalue.extended;
        f := ref(bigtable[lowervalue.typeindex]);

        if (lowervalue.typeindex <> noneindex) and
           (uppervalue.typeindex <> noneindex) and
           ((lowervalue.typeindex <> uppervalue.typeindex) or
           not (f^.typ in [ints, chars, bools, scalars]) or (extended and
           ((sharedPtr^.switchcounters[standard] > 0) or lowervalue.cvalue.negated or
           ((lowervalue.cvalue.intvalue < 0) and
           (lowervalue.cvalue.intvalue > uppervalue.cvalue.intvalue)))) or
           (not extended and (((lowervalue.cvalue.intvalue < 0) and
           not lowervalue.cvalue.negated) or (lowervalue.cvalue.intvalue > uppervalue.cvalue.intvalue)))) then
          warnbefore (badsubrange)

        else
          begin
          enterform (subranges, resulttype, resulp);
          with resulp^ do
            begin
            lowerord := lowervalue.cvalue.intvalue;
            upperord := uppervalue.cvalue.intvalue;
            parentform := f^.typ;

            { The next section is parameterized to set the allocation size for variables, depending on the target machine }
            size := defaulttargetintsize;
            if not extended then
              if lowerord < 0 then
                if simplesize (max (abs(lowerord + 1), abs(upperord))) = defaulttargetintsize then
                  size := defaulttargetintsize
                else
                  size := simplesize (max(abs(lowerord + 1), abs(upperord)) * 2)
              else
                size := simplesize (upperord);
            align := size;
            parenttype := lowervalue.typeindex;

            p := ref(bigtable[parenttype]);
            { the strategy here is to allocate 2 bytes if it is subrange of a parenttype which is going to be 4 bytes }
            size := min (p^.size, forcealign(size, intalign, false));
            align := p^.align;
            extendedrange := extended;
            end; {with resulp}
          end;
        end;
        {>>>}
      end;

    verify1 (follow, badtypesyntax);
  end;
  {>>>}

begin
  if token = packedsym then
    begin
    packflag := true;
    gettoken
    end
  else
    packflag := false;

  if token in begstructset then
    begin
    case token of
      uparrow:   ptrtyp;
      arraysym:  arraytyp;
      filesym:   filetyp;
      setsym:    settyp;
      recordsym: recordtyp;
      stringsym: stringtyp;
      end;
    end
  else
    simpletyp;

  verify1 (follow, badtypesyntax);

  resulp := ref(bigtable[resulttype]);
  anyfile := anyfile or resulp^.containsfile;
end;
{>>>}

{<<<}
procedure labeldefinition;
{<<<}
{ Syntactic routine to parse a label-declaration-part.

  Productions:

  label-declaration-part = "label" label [* "," label *] ";"  .

  Label declarations are recorded in labelentry's which are linked
  on a list rooted in "labellist" in the display entry.

  Each label has an internal label value assigned from a counter.

  The source line and column of the label are recorded to allow
  diagnostics if the label is not defined.

  Definednest is initialized to 0 (undefined)
  Maxlegalnest is initialized to maxint (any nest level)
}
{>>>}

var
  t: labelptr; {temp ptr with various uses}
  value1: integer; {label value}

  {<<<}
  procedure onelabeldef;
  { Process a single label definition, checking for duplicate label
    declarations and entering the declaration in the label list
  }
  begin
    if token <> intconst then
      warnbetween(nolabelerr)
    else
      begin
      value1 := thistoken.intvalue;
      if value1 > maxstandardlabel then
        warn (biglabelerr);

      with display[level] do
        begin
        searchlsection (value1, labellist, t);

        if t <> labelflag then
          warn (duplabeldef)
        else
          begin
          new (t);
          with t^ do
            begin
            labelvalue := value1;
            nextlabel := labellist;
            labellist := t;
            maxlegalnest := maxint;
            definednest := 0;
            internalvalue := sharedPtr^.lastlabel;
            { code generators need two entry points for Pascal labels }
            sharedPtr^.lastlabel := sharedPtr^.lastlabel - 2;
            nonlocalref := false;
            with thistoken do
              begin
              labelline := line;
              labelcolumn := (left + right) div 2;
              end;
            end
          end;
        end;

      gettoken;
      end;
  end;
  {>>>}

begin
  gettoken;
  onelabeldef;

  while token in [comma, intconst] do
    begin
    verifytoken (comma, nocommaerr);
    onelabeldef
    end;

  verifytoken (semicolon, nosemiheaderr);
  verify1 (neverskipset, baddeclerr);
end;
{>>>}
{<<<}
procedure constantdefinition;
{ Syntactic routine to parse a constant definition part.
  Productions:
  constant-definition-part = "const" constant-definition ";"
        [* constant-definition ";" *]
  constant-definition = identifier "=" constant  .
  Constants simply have their value entered in the name table.
  }
var
  t: tableIndex; { constant identifier entry}
  p: entryptr; {used for nametable access}
  newvalue: operand; {value of the constant}
  startpos: integer; {position of first var in source file}
  startline: integer; {line number of first var}

begin
  startpos := thistoken.filepos;
  startline := thistoken.line;
  gettoken;
  repeat
    enterlocalident (t, noname);
    verifytoken(eql, noeqlerr);
    constant (neverskipset, true, newvalue);
    p := ref(bigtable[t]);
    with p^ do
      begin
      namekind := constname;
      consttype := newvalue.typeindex;
      constvalue := newvalue.cvalue;
      if consttype = nilindex then warnnonstandard(badconsterr);
      end;

    verifytoken(semicolon, nosemiheaderr);
    verify1(constfollowset + neverskipset, baddeclerr);
  until not (token in constfollowset);
end;
{>>>}
{<<<}
procedure typedefinition;
{ Syntactic routine to parse a type-definition-part.
  Productions:
  type-definition-part = "type" type-definition ";"
        [* type-definition ";" *]  .
  type-definition = identifier "=" type-denoter  .
  As each type is parsed its formentry is created in main store,
  and its nameentry contains the index of the formentry.
}
var
  t: tableIndex; { name of type being defined }
  p: entryptr; { used for name access }
  f: tableIndex; {form entry for type being defined}
  startpos: integer; {position of first var in source file}
  startline: integer; {line number of first var}

begin
  gettoken;
  repeat
    startpos := thistoken.filepos;
    startline := thistoken.line;

    enterlocalident (t, undeftypename);
    verifytoken (eql, noeqlerr);

    gettyp([eql, ident, semicolon, uparrow..stringsym, nilsym, intconst..stringconst, plus, minus, lpar], f);
    p := ref(bigtable[t]);
    with p^ do
      begin
      namekind := typename;
      typeindex := f;
      refdefined := false;
      end;

    verifytoken (semicolon, nosemiheaderr);
    verify1 (typefollowset + neverskipset, baddeclerr);
  until not (token in typefollowset);
end;
{>>>}
{<<<}
procedure vardefinition (sharedvar: boolean);
{ Syntactic routine to parse a variable-declaration-part.
  Productions:
  variable-declaration-part = "var" variable-declaration ";"
               [* variable-declaration ";" *]  .
  variable-declaration = identifier [* "," identifier *] ":"
        type-denoter  .
  Almost all of the work is actually done in "variablelist"
}
var
  a: alignmentrange; {default alignment}

begin
  gettoken;
  with display[level] do
    variablelist (neverskipset, [], blockid, dbgscope, blocksize, a, varname, true, false, sharedvar);
end;
{>>>}
{<<<}
procedure changeparamids (firstparam: tableIndex; lastparam: tableIndex; n: integer);
{ Scan down the parameters of a procedure and change their scope to the
  specified scope.  This is used to change forward parameter declarations
  to the current scope, or to make them inaccessable after the declaration.
  Skips over items with name = deadscope, to avoid rejuvenating killed
  parameters nested within procedure parameters.
}
var
  p: entryptr; {provides nametable access for parameters}
  i: tableIndex; {for stepping through parameters}

begin
  for i := firstparam to lastparam do
    begin
    p := ref(bigtable[i]);
    if not p^.form and (p^.name <> deadscope) then
      p^.name := n;
    end;
end;
{>>>}
{<<<}
procedure getfunctiontype (functiondefinition: boolean; forwardbody: boolean; var returntype: tableIndex);
{ Syntactic routine to parse a function result type if necessary.
  Production:
  function-type = [ ":" type-identifier ]  .
  If we are parsing a function, and it is not a forward body for
  a previously defined function, this routine gets the result type.
  A procedure or forward function will have type "noneindex".
  If the type is not legal for a function, an error will be emitted.
}
var
  f: entryptr; {for access to returntype}
  t: tableIndex; {for finding type identifier}
  i: levelindex; {for stepping through levels looking for it}

begin
  returntype := noneindex;
  if (functiondefinition and not forwardbody) or (token = colon) then
    begin
    if not functiondefinition then
      warn(badcolonerr)
    else if forwardbody then
      warn(dupfwdresult);

    verifytoken (colon, nocolonerr);

    if token = ident then
      begin
      i := displaytop; {Name must be found in enclosing scopes, not current}
      repeat
        i := i - 1;
        searchsection(display[i].blockid, t);
      until (i = 0) or (t <> 0);
      f := ref(bigtable[t]);
      if f^.namekind = typename then
        returntype := f^.typeindex
      else
        warn (notypenameerr);
      gettoken;
      end
    else
      begin
      warn (notypenameerr);
      gettyp (neverskipset + [rpar], returntype);
      end;

    f := ref(bigtable[returntype]);
    if (not (f^.typ in legalfunctypes)) and
       (sharedPtr^.switchcounters[standard] > 0) then
      warn(badfunctype);
    end;
end;
{>>>}
{<<<}
procedure parameterdefinition (var paramsize: addressrange; follow: tokenset);
{ Syntactic routine to parse a parameter list.
  Productions:
  formal-parameter-list = "(" formal-parameter-section
        [* ";" formal-parameter-section *] ")"  .
  Parameters are parsed and allocated at the top of the frame for
  the procedure, in such an order that they are pushed onto the stack
  in the order in which they are declared.
  Each value parameter is allocated the space for a value, each var
  parameter has space for a pointer to the parameter, and each
  routine parameter has space for the routine address and a pointer
  for the static link.
}
  {<<<}
    procedure oneparampiece;

  { Syntactic routine to parse one formal parameter section.

    Productions:

    formal-parameter-section = value-parameter-specification |
          variable-parameter-specification |
          routine-parameter-specification  .

    value-parameter-specification = variable-list  .

    variable-parameter-specification = "var" variable-list  .

  }


      procedure routineparam(routinekind: nametype {parameter kind} );

  { Syntactic routine to parse a routine-parameter-section.

    Productions:

    routine-parameter-section = ( "procedure" | "function " )
          [ parameter-list ] [ function-type ] ";"  .

    Parameters are assigned a new scope id, and will be unlinked from the
    key map on exit from the actual routine block.  They will, however,
    remain in the name table for checking when a routine is passed as a
    parameter.
  }

        var
          routineindex: tableIndex; {entry for the parameter routine}
          p: entryptr; {used for name access}
          returntype: tableIndex; {type if function}
          intleveldummy: boolean; {actually has double use}
          sizedummy: addressrange; {dummy param to parameterdefinition}
          t: integer; {temp storage for last id}


        begin {routineparam}
          gettoken;
          sizedummy := 0;
          onevar(lastid, routinekind, routineindex, false);
          t := lastid;
          if (paramlistid + 1) >= totalscopes then
            analysFatal (manyscopes); {the last value of totalscopes is not used.}
          lastid := paramlistid + 1;
          paramlistid := lastid;
          if token = lpar then
            parameterdefinition(sizedummy,
                                [colon, rpar, semicolon] + begparamhdr);
          p := ref(bigtable[routineindex]);
          p^.lastinsection := true;
          p^.offset := paramsize;
          p^.length := procparamsize;
          paramsize := paramsize + procparamsize;
          p^.nextparamlink := tabletop;
          intleveldummy := routinekind = funcparam;
          getfunctiontype(intleveldummy, false, returntype);
          p^.vartype := returntype;
          lastid := t;
          changeparamids(routineindex + 1, tabletop, deadscope);
          verify([rpar, semicolon], begparamhdr + neverskipset, badparamerr);
          if token = semicolon then gettoken;
        end {routineparam} ;


      procedure conformantparam(paramkind: nametype; {kind of parameter}
                                var result: tableIndex {resulting parameter type} );

  { Parse a conformant array parameter declaration.
    Boundid's are allocated after the parameter.
    If we are unable to parse a boundid, a dummy entry is used, since the
    pass gets very confused later if the boundid entries are not,
    in fact, boundid's.
  }

        var
          packflag: boolean; {true if packed conformant array}
          highid, lowid: tableIndex; {high and low bound id's}
          thisindextype: tableIndex; {index type for this array}
          p: entryptr; {for access to names and forms}
          resulp: entryptr; {for access to result form}
          elttype: tableIndex; {element type}
          lasttabletop: tableIndex; {initial value of tabletop}

        begin {conformantparam}
          packflag := false;
          if (token = packedsym) or (token = arraysym) then
            begin
            if sharedPtr^.switchcounters[level0] > 0 then warnnonstandard(notlevel0);
            if token = packedsym then
              begin
              packflag := true;
              gettoken;
              end;
            if token = arraysym then
              begin
              gettoken;
              if token = lpar then
                begin
                warn(nolbrackerr);
                gettoken;
                end
              else verifytoken(lbrack, nolbrackerr);
              end
            end
          else verifytoken(semicolon, nosemiheaderr);

          lowid := 0;
          lasttabletop := tabletop;
          if token in [intconst, charconst] then
            begin
            warn(novarerr);
            gettoken;
            end
          else onevar(lastid, boundid, lowid, false);
          if lowid = 0 then lowid := nullboundindex;
          verifytoken(dotdot, nodotdoterr);
          highid := 0;
          lasttabletop := tabletop;
          if token in [intconst, charconst] then
            begin
            warn(novarerr);
            gettoken;
            end
          else onevar(lastid, boundid, highid, false);
          if highid = 0 then highid := nullboundindex;
          verifytoken(colon, nocolonerr);
          if token <> ident then warn(notypenameerr);
          gettyp(follow + [semicolon, rbrack, ofsym, arraysym], thisindextype);
          p := ref(bigtable[thisindextype]);
          if not (p^.typ in [none, chars, bools, scalars, subranges, ints]) then
            warn(badindex);

          if token = rpar then
            begin
            warn(norbrackerr);
            gettoken;
            end
          else if token <> semicolon then
            begin
            verifytoken(rbrack, norbrackerr);
            verifytoken(ofsym, nooferr);
            end;

          if packflag and (token <> ident) then warn(badpackconform);
          if token in [semicolon, packedsym, arraysym] then
            conformantparam(paramkind, elttype)
          else
            begin
            if token <> ident then warn(notypenameerr);
            gettyp(nextparamhdr + [semicolon], elttype);
            end;

          enterform(conformantarrays, result, resulp);
          with resulp^ do
            begin
            packedflag := packflag;
            size := 0;
            align := ptralign;
            lowbound := lowid;
            highbound := highid;
            indextype := thisindextype;
            elementtype := elttype;
            stringtype := false;
            p := ref(bigtable[elttype]);
            containsfile := p^.containsfile;
            elementsize := arraysizeof(p, packflag);
  { the following test should follow the wry logic in arraywork }
            if packflag and (elementsize > bitsperunit * packingunit) then
              elementsize := arraysizeof(p, false) * bitsperunit;
            end;
          p := ref(bigtable[lowid]);
          p^.sparelink := result;
          p := ref(bigtable[highid]);
          p^.lastinsection := true;
          p^.sparelink := 0; {used only by lowid}
        end {conformantparam} ;


      procedure allocboundids(paramtype: tableIndex {parameter type} );

  { Allocate the boundid's for a conformant parameter.  This is a separate
    action because they must follow the parameter itself in order for the
    call to work.
  }

        var
          elttype: tableIndex; {elementtype for this conformant array schema}
          indextype: tableIndex; {indextype for this schema}
          p: entryptr; {for access to paramtype}
          highid, lowid: tableIndex; {boundid names}
          indexlen: addressrange; {length of indextype}
          a: alignmentrange; {alignment for this type}
          align: alignmentrange; {parameter alignment (dummy)}

        begin {allocboundids}
          align := intalign;
          repeat
            p := ref(bigtable[paramtype]);
            elttype := p^.elementtype;
            indextype := p^.indextype;
            lowid := p^.lowbound;
            highid := p^.highbound;
            p := ref(bigtable[indextype]);
            getallocdata(p, boundid, false, paramsize, indexlen, a, align);
            alloconevar(lowid, indextype, boundid, paramsize, a, indexlen,
                        false, false);
            alloconevar(highid, indextype, boundid, paramsize, a, indexlen,
                        false, false);
            paramtype := elttype;
            p := ref(bigtable[paramtype]);
          until p^.typ <> conformantarrays;
        end {allocboundids} ;


      procedure oneparamlist(paramkind: nametype {kind of parameter list} );

  { Syntactic routine to parse a value or variable parameter section.

    Productions:

    variablelist = identifier [* "," identifier *] ":"
                   typeidentifier | conformantspecification  .

    This cannot be handled by the normal variables procedure because of
    the strange processing required for conformant arrays.
  }

        var
          a: alignmentrange; {alignment if this var}
          align: alignmentrange; {"parameter" alignment}
          first, last: tableIndex; {limits of parameters}
          paramtype: tableIndex; {type of these paramters}
          paramptr, p: entryptr; {used for access to table entries}
          t: tableIndex; {induction var}
          typelen: addressrange; {space allocated for the var}
          univflag: boolean; {true if universal parameter}
          rp: boolean; {true if parameter passed by reference}

        begin {oneparamlist}
          align := unitsize;
          verify([ident], nextparamhdr, novarerr);

          onevar(lastid, paramkind, first, false);
          last := first;
          while token in [ident, comma] do
            begin
            verifytoken(comma, nocommaerr);
            onevar(lastid, paramkind, last, false);
            end;

          paramtype := noneindex;

          verifytoken(colon, nocolonerr);

          univflag := false;

          if token in [packedsym, arraysym] then
            begin
            if paramkind = varparam then paramkind := varconfparam
            else paramkind := confparam;
            conformantparam(paramkind, paramtype);
            end
          else if token in [ident, univsym] then
            begin
            if token = univsym then
              begin
              if paramkind <> varparam then warn(baduniv);
              gettoken;
              univflag := true;
              end;
            search(paramtype);
            paramptr := ref(bigtable[paramtype]);
            if paramptr^.namekind <> typename then
              begin
              warn(notypenameerr);
              gettoken;
              paramtype := noneindex;
              end
            else gettyp(nextparamhdr + [semicolon, rpar], paramtype);
            end
          else warn(notypenameerr);

          paramptr := ref(bigtable[paramtype]);
          if paramptr^.containsfile and (paramkind in [param, confparam]) then
            warn(novaluefile);

          for t := first to last do
            begin
            paramptr := ref(bigtable[paramtype]);
            getallocdata(paramptr, paramkind, false, paramsize, typelen, a,
                         align);
            if (paramkind = param) then
              if (typelen > maxparambytes) and
                 (paramptr^.typ in [sets, arrays, fields{, strings}]) then
                begin
                typelen := sharedPtr^.ptrsize;
                a := ptralign;
                rp := true;
                end
              else rp := false
            else rp := true;
            alloconevar(t, paramtype, paramkind, paramsize, a, typelen, rp,
                        false);
            if univflag then
              begin
              p := ref(bigtable[t]);
              p^.univparam := true;
              end;
            end;
          if paramkind in [varconfparam, confparam] then
            allocboundids(paramtype);
          paramptr := ref(bigtable[last]);
          paramptr^.lastinsection := true;
          paramptr^.nextparamlink := tabletop;
          if token = semicolon then gettoken
          else
            verify(nextparamhdr, follow + begtypset + [comma, colon, semicolon],
                   nosemiheaderr);

          paramsize := forcealign(paramsize, stackalign, false);
        end {oneparamlist} ;


      begin {oneparampiece}
        verify(begparamhdr, [rpar, semicolon, comma], badparamerr);
        case token of
          functionsym: routineparam(funcparam);
          varsym:
            begin
            gettoken;
            oneparamlist(varparam)
            end;
          ident: oneparamlist(param);
          proceduresym: routineparam(procparam);
          end;
      end {oneparampiece} ;
  {>>>}

begin
  gettoken;
  oneparampiece;

  while token in ([comma, semicolon, functionsym, proceduresym, varsym, ident]) do
    begin
    if lasttoken.token <> semicolon then
      warnbetween (nosemiheaderr);
    oneparampiece;
    end;

  if lasttoken.token = semicolon then
    warnbefore (badparamerr);

  verifytoken (rpar, norparerr);
  verify1 (neverskipset + follow, badparamerr);
end;
{>>>}
{<<<}
procedure procdefinition;
{<<<}
{ Syntactic routine to parse a procedure or function definition.
  Productions:
  routine-declaration = ( "procedure" | "function" ) identifier
        [ formal-parameter-definition ] function-type ";"
        ( block | directive ) ";"
  The routine name is defined with the scope id of the enclosing block,
  and enters a new scope for the parameters.  This scope continues through
  the end of the procedure block (if any).  Each procedure defined is also
  described in a separate table called the "proctable" which saves basic
  data on the procedure.  This is kept in memory because it has frequent
  write access, and because it is larger than the other name entries, and
  would expand the nameentry unnecessarily.

  If the declaration is for a forward or external procedure, the parameters
  are saved in the name table with the scope id for the procedure, and this
  id will not be reused as there is no block exit.  When the forward body
  is encountered, the scope id's of these parameters is changed to the scope
  id of the body.  Upon exit from the body, there is no way to unlink these
  forward defined parameters as is done for normal routines, so they have
  their scope id changed to a special value which will not be used by
  normal scopes.  This effectively makes them inaccessable.

  When a function returns a real result, if the function does not use
  Pascal linkage, then this must be communicated to genblk to allow proper
  access to the value coming back in a register (i.e. is it r0 or f0?).
}
{>>>}

{<<<}
var
  functiondefinition: boolean; {parsing a function}
  forwardbody: boolean; {proc was forward defined}
  procptr: entryptr; {provides access to procedure name entry}
  procindex: tableIndex; {procedure name entry}
  paramindex: tableIndex; {start of parameter entries}
  returntype: tableIndex; {function result type formentry}
  directive: standardids; {which directive, forward etc}
  directiveindex: tableIndex; {and symbol table index thereof}
  funcline: integer; {line number where function defined}
  funccol: columnindex; {col where func defined (for not assigned msg)}
  f: entryptr; {used for access to returntype}
  hasparameters: boolean; {true if there are parameters}
{>>>}

begin
  hasparameters := false;
  functiondefinition := token = functionsym;
  gettoken;
  forwardbody := false;
  funcline := thistoken.line;
  funccol := (thistoken.left + thistoken.right) div 2;

  if token = ident then
    begin
    searchlevel(procindex);
    if procindex <> 0 then
      begin
      procptr := ref(bigtable[procindex]);
      with procptr^ do
        begin
        forwardbody := namekind in [externalproc, externalfunc, forwardproc, forwardfunc];
        if functiondefinition and (namekind in [externalproc, forwardproc]) then
          warn (fwdprocfuncerr)
        else if not functiondefinition and (namekind in [externalfunc, forwardfunc]) then
          warn (fwdfuncprocerr);
        end;
      end;
    end;

  if not forwardbody then
    begin
    enterlocalident(procindex, noname);
    procptr := ref(bigtable[procindex]);
    procptr^.procref := newproc;
    sharedPtr^.proctable[procptr^.procref].backlink := display[level].blockref;
    end
  else
    gettoken;

  if level = maxlevel then
    analysFatal (levelerr);
  procptr := ref(bigtable[procindex]);
  enterblock (level + 1, procindex, procptr^.procref);

  display[level+1].dbgscope := procptr^.dbgsymbol;

  if token = lpar then
    {<<<  lpar}
    begin
    hasparameters := true;
    if forwardbody then
      warn (dupfwdparam);
    paramlistid := lastid;
    parameterdefinition (display[level + 1].paramsize, [colon]);
    end;
    {>>>}
  paramindex := tabletop;
  getfunctiontype (functiondefinition, forwardbody, returntype);

  verifytoken (semicolon, nosemierr);
  procptr := ref (bigtable[procindex]);
  with procptr^ do
    begin
    sharedPtr^.proctable[procref].globaldeath := false;
    sharedPtr^.proctable[procref].charindex := charindex;
    sharedPtr^.proctable[procref].charlen := min(maxprocnamelen, charlen);
    f := ref (bigtable[returntype]);
    { Reset the calling linkage to Pascal-2.  This is needed
      for non-pascal declared routines for which a body is present.
      Not needed on systems that support non-pascal procedure bodies.
      Remember, calllinkage is initialized to pascal2call in newproc.
      Interruptcall procedures are an exception. }
    if not (sharedPtr^.proctable[procref].calllinkage = interruptcall) then
      sharedPtr^.proctable[procref].calllinkage := pascal2call;

    if forwardbody then
      begin
      display[level + 1].paramsize := savedparamsize;
      end

    else
      begin
      sharedPtr^.proctable[procref].realfunction := ((f^.typ = reals) or (f^.typ = doubles)) and functiondefinition;
      sharedPtr^.proctable[procref].intlevelrefs := false;
      sharedPtr^.proctable[procref].level := level + 1;
      funclen := f^.size;
      functype := returntype;
      paramlist := paramindex
      end;

    if token = ident then
      {<<<  ident}
      begin
      search (directiveindex);

      f := ref(bigtable[directiveindex]);
      if f^.namekind = directivename then
        begin
        directive := f^.procid;
        if directive = forwardid then
          {<<<  forward}
          begin
          if forwardbody then
            warn (dupforward)
          else
            enterundef (procindex);
          if functiondefinition then
            namekind := forwardfunc
          else
            namekind := forwardproc
          end
          {>>>}
        else
          {<<<  external, nonpascal, fortran, interrupt}
          begin
          if forwardbody then
            warn(dupforward);
          if functiondefinition then
            namekind := externalfunc
          else namekind := externalproc;

          if directive = nonpascalid then
            sharedPtr^.proctable[procref].calllinkage := nonpascalcall
          else if directive = fortranid then
            sharedPtr^.proctable[procref].calllinkage := fortrancall
          else if not functiondefinition and (directive = interruptid) then
            begin
            sharedPtr^.proctable[procref].calllinkage := interruptcall;
            if hasparameters or (level <> 1) then
              warn (badinterruptproc);
            end;

          anyexternals := true;
          sharedPtr^.proctable[procref].externallinkage := true;
          sharedPtr^.proctable[procref].globaldeath := true;
          if level <> 1 then
            warn(badxdef);
          end;
          {>>>}
        end
      else
        warn (baddirective);

      gettoken;

      with display[level + 1] do
        begin
        savedparamsize := paramsize;
        display[level].namesdeclared := display[level].namesdeclared + namesdeclared;
        display[level].highestkey := max(display[level].highestkey, highestkey);
        end;
      end
      {>>>}
    else
      {<<<  no directive}
      begin
      if functiondefinition then
        namekind := funcname
      else
        namekind := procname;

      funcassigned := not functiondefinition;

      if forwardbody then
        changeparamids(procindex + 1, paramlist, lastid)
      else
        begin
        { bump lastscope as parameter list lives a reality separate than the procedure's block! }
        if lastscope >= totalscopes then
          analysFatal (manyscopes);
        lastscope := lastscope + 1;
        display[level + 1].scopeid := lastscope;
        end;
      display[level + 1].oldtabletop := tabletop;

      if procref <= cseregions then
        begin
        sharedPtr^.cseregiontable[procref, false].low := maxaddr;
        sharedPtr^.cseregiontable[procref, false].high := 0;
        sharedPtr^.cseregiontable[procref, true].low := maxaddr;
        sharedPtr^.cseregiontable[procref, true].high := 0;
        end;
      level := level + 1;
      block;

      procptr := ref(bigtable[procindex]);

      level := level - 1;
      if not procptr^.funcassigned then
        warnat (nofuncass, funcline, funccol);

      sharedPtr^.proctable[procptr^.procref].bodydefined := true;
      directiveindex := procptr^.paramlist; {cache buffer...}

      if forwardbody then
        changeparamids(procindex + 1, directiveindex, deadscope);

      for directiveindex := procindex + 1 to directiveindex do
        begin
        f := ref(bigtable[directiveindex]);
        if not f^.form then f^.modified := f^.parammodified;
        end;
      end;
      {>>>}
    end;

  displaytop := level;
  verifytoken (semicolon, nosemiprocerr);
end;
{>>>}

{<<<}
procedure block;
{ Syntactic routine to parse a block, contains most of syntax analyzer }

var
  firstsection: boolean; {set if first data declaration at this level}
  firstprocdefined: boolean; {false until a proc/func is defined at this level}

begin
  verify1 (begblockset, blockstarterr);

  firstsection := true;
  firstprocdefined := false;
  repeat
    if not firstsection then
      warnnonstandard(scrambledblkerr);

    if token = labelsym then
      labeldefinition;

    if token = constsym then
      constantdefinition;

    if token = typesym then
      typedefinition;

    if token = sharedsym then
      begin
      if level > 1 then
        warn(badsharedvar);
      vardefinition (true);
      end;

    if token = varsym then
      begin
      if (level > 1) and firstprocdefined then
        warn(scrambledblkerr);
      vardefinition (false);
      end;

    firstsection := false;
    while token in [functionsym, proceduresym] do
      begin
      listundeftypes;
      if not firstprocdefined then fixupparamoffsets(level > 1);
      firstprocdefined := true;
      procdefinition;
      end;
  until not (token in blockheadset);

  if not firstprocdefined or (level <= 1) then
    fixupparamoffsets (true);

  listundeftypes;
  listundefprocs;
  genstmt (begblk);
  genint (thistoken.line);

  with display[level] do
    begin
    genint (blockref);
    if level = 1 then
      sharedPtr^.globalsize := blocksize;
    genint (paramsize);
    genint (blocksize);
    genint (thistoken.baseLine);
    end;

  loopfactor := 0; { not in a loop }
  anynonpascalcalls := false;
  anynonlocallabels := false;
  forsp := 0;

  nowdebugging := (sharedPtr^.switchcounters[debugging] > 0) or
                  (sharedPtr^.switchcounters[profiling] > 0);

  if (level > 1) or (sharedPtr^.switchcounters[mainbody] > 0) or (token <> eofsym) then
    begin
    if level = 1 then
      begin
      sharedPtr^.proctable[0].bodydefined := true; { use this entry for main body }
      if (sharedPtr^.switchcounters[mainbody] <= 0) and (token <> eofsym) then
        warn (extrastmterr);
      end;
    body;
    verifytoken (endsym, noenderr);
    end;

  verify1 (neverskipset + [dot], blockenderr);

  listundeflabels;
  genstmt (endblk);
  exitblock (level);
end;
{>>>}

{<<<}
procedure analys;
{<<<}
{ Syntactic routine to parse and generate intermediate file output for a Pascal program.
  program = [ program-heading  ";" ] block  .
  program-heading = "program" identifier " [ "(" identifier-list ")" ]  .
  This routine is responsible for program initialization, parsing of the entire program, and final cleanup.
  The majority of the work is done by the routine "block" }
{>>>}

  {<<<}
  procedure init;
  { Initialize all tables, etc. }
  {<<<}
  type
    standardstring = packed array [1..9] of char; {used for standard ids}
    kludgerecord =
      record
        case boolean of
          false: (p: integer {targep} {nil pointer value} );
          true: (b: packed array [1..32] of hostfilebyte);
      end;
  {>>>}
  {<<<}
  var
    f: tableIndex;  { used in creating forms}
    fptr: entryptr; { used to access forms being created}
    p: entryptr;    { used to access name entries}
    i: integer;     { Misc induction vars}
    kludge: kludgerecord; { for reversing bytes of NIL constant}
    b: hostfilebyte; { temp for reversing bytes}
  {>>>}

    {<<<}
    procedure setId (id: standardids; n: nametype; len: addressrange; f: tableIndex);
    { Make a name table entry for a standard identifier}

    var
      i: integer;

    begin
      tabletop := tabletop + 1;
      i := sharedPtr^.standardidtable[id];
      keymap[i] := tabletop;

      p := ref(bigtable[tabletop]);
      with p^ do
        begin
        form := false;
        name := lastid;
        lastoccurrence := lastscope;
        nextname := 0;
        namekind := n;
        charindex := 1;
        charlen := 0;

        with display[displaytop] do
          if n <> boundid then
            begin
            namesdeclared := namesdeclared + 1;
            if i > highestkey then
              highestkey := i;
            end;

        case n of
          typename:
            typeindex := f;

          varname, boundid:
            begin
            offset := display[level].blocksize;
            length := len;
            if n = varname then
              display[level].blocksize := display[level].blocksize + length;
            vartype := f;
            knownvalid := true;
            modified := true;
            programdecl := false;
            univparam := false;
            varalloc := normalalloc;
            end;

          directivename, standardproc, standardfunc:
            procid := id;

          constname:
            consttype := f;
          end;

        end;
    end;
    {>>>}

  begin
    { This code checks certain configuration parameters and reports any potential problems. }
    if tablesize < hashtablesize then
      {<<<  tablesize (a field width) not as large as hashtablesize, give up}
      begin
      write ('Tablesize is smaller than hashtablesize');
      abort (inconsistent);
      end;
      {>>>}

    {<<<  init token lengths}
    toklengths[programsym] := 6;
    toklengths[labelsym] := 4;
    toklengths[constsym] := 4;
    toklengths[typesym] := 3;
    toklengths[varsym] := 2;
    toklengths[proceduresym] := 8;
    toklengths[functionsym] := 7;
    toklengths[uparrow] := 0;
    toklengths[arraysym] := 4;
    toklengths[filesym] := 3;
    toklengths[setsym] := 2;
    toklengths[recordsym] := 5;
    toklengths[stringsym] := 5;
    toklengths[univsym] := 3;
    toklengths[packedsym] := 5;
    toklengths[originsym] := 5;
    toklengths[usesym] := 2;
    toklengths[definesym] := 5;
    toklengths[sharedsym] := 5;
    toklengths[beginsym] := 4;
    toklengths[ifsym] := 1;
    toklengths[casesym] := 3;
    toklengths[whilesym] := 4;
    toklengths[repeatsym] := 5;
    toklengths[forsym] := 2;
    toklengths[withsym] := 3;
    toklengths[gotosym] := 3;
    toklengths[eql] := 0;
    toklengths[lss] := 0;
    toklengths[gtr] := 0;
    toklengths[neq] := 1;
    toklengths[leq] := 1;
    toklengths[geq] := 1;
    toklengths[insym] := 1;
    toklengths[plus] := 0;
    toklengths[minus] := 0;
    toklengths[orsym] := 1;
    toklengths[star] := 0;
    toklengths[slash] := 0;
    toklengths[divsym] := 2;
    toklengths[modsym] := 2;
    toklengths[andsym] := 2;
    toklengths[ofsym] := 1;
    toklengths[endsym] := 2;
    toklengths[elsesym] := 3;
    toklengths[thensym] := 3;
    toklengths[otherwisesym] := 8;
    toklengths[dosym] := 1;
    toklengths[untilsym] := 4;
    toklengths[tosym] := 1;
    toklengths[downtosym] := 5;
    toklengths[notsym] := 2;
    toklengths[at] := 0;
    toklengths[nilsym] := 2;
    toklengths[colon] := 0;
    toklengths[dot] := 0;
    toklengths[dotdot] := 1;
    toklengths[comma] := 0;
    toklengths[semicolon] := 0;
    toklengths[becomes] := 1;
    toklengths[lpar] := 0;
    toklengths[rpar] := 0;
    toklengths[lbrack] := 0;
    toklengths[rbrack] := 0;
    toklengths[intconst] := 0;
    toklengths[realconst] := 0;
    toklengths[dblrealconst] := 0;
    toklengths[charconst] := 0;
    toklengths[stringconst] := 0;
    toklengths[ident] := 0;
    toklengths[eofsym] := 0;
    toklengths[lineinc] := 0;
    toklengths[lineadd] := 0;
    toklengths[newfile] := 0;
    {>>>}
    emitflag := true;
    emptysetgenerated := false;
    checkundefs := true;
    linearize := false;
    skipfactor := false;
    divfolded := false;
    sharedPtr^.lastlabel := 32766;
    nextintcode := 0;
    {<<<  init sets}
    blockheadset := [labelsym..functionsym];
    begblockset := [labelsym..functionsym, beginsym];
    begparamhdr := [functionsym, proceduresym, varsym, ident];
    nextparamhdr := [functionsym, proceduresym, varsym, ident, rpar];
    begstmtset := [beginsym..gotosym, ident];
    begunsignedset := [nilsym, ident, intconst, charconst, realconst, dblrealconst, stringconst];
    begconstset := [nilsym, ident, intconst, charconst, realconst, dblrealconst, stringconst, plus, minus];
    begstructset := [uparrow..stringsym];
    begsimplset := [nilsym, ident, intconst, charconst, realconst, dblrealconst, stringconst, plus, minus, lpar];
    begtypset := [uparrow..stringsym, nilsym, ident, intconst, charconst, realconst, dblrealconst, stringconst,
                  plus, minus, lpar];
    exprops := [eql..insym];
    sexprops := [plus..orsym];
    termops := [star..andsym];
    begfactset := [ident, intconst, realconst, dblrealconst, charconst, stringconst, lbrack, lpar, notsym, at, nilsym];
    begexprset := [eql..andsym, ident, intconst, realconst, dblrealconst, charconst, stringconst, lbrack, lpar, notsym, nilsym];
    legalfunctypes := [scalars, ints, reals, doubles, bools, chars, ptrs, subranges, none];
    neverskipset := [labelsym..functionsym, beginsym..gotosym, ident, semicolon, eofsym, endsym];
    constfollowset := [ident, semicolon, eql, plus, minus, lpar, nilsym, intconst..stringconst];
    typefollowset := [ident, semicolon, eql, uparrow..stringsym, nilsym, plus, minus, lpar, intconst..stringconst];
    {>>>}
    {<<<  init map for binary operators}
    optable[eql] := eqop;
    optable[lss] := lssop;
    optable[gtr] := gtrop;
    optable[neq] := neqop;
    optable[leq] := leqop;
    optable[geq] := geqop;
    optable[insym] := inop;
    optable[plus] := plusop;
    optable[minus] := minusop;
    optable[orsym] := orop;
    optable[star] := mulop;
    optable[slash] := slashop;
    optable[divsym] := quoop;
    optable[modsym] := remop;
    optable[andsym] := andop;
    {>>>}
    intstate := stmtstate;

    with tokenSharedPtr^.nexttoken do
      begin
      left := 0;
      right := 0;
      line := 1
      end;

    probing := false;
    sourcestringindex := 0;
    fewestblocks := lastblocksin;
    mostblocks := lastblocksin;
    {<<<  init csregiontable}
    for i := 0 to cseregions do
      begin
      with sharedPtr^.cseregiontable[i, false] do
        begin
          low := 0;
          high := maxaddr;
        end;
      with sharedPtr^.cseregiontable[i, true] do
        begin
          low := 0;
          high := maxaddr;
        end;
      end;
    {>>>}

    for i := 0 to hashtablesize do
      keymap[i] := 0;

    sharedPtr^.lastvartableptr := 0;
    sharedPtr^.lastvartableentry := 0;

    scanToken;
    gettoken;

    nullboundindex := 0;
    inputdeclared := false;
    new (labelflag);

    lastdebugrecord := 0;

    sharedPtr^.stringblkptr := sharedPtr^.stringblkptrtbl[sharedPtr^.curstringblock];
    if sharedPtr^.stringblkptr = nil then
      begin
      new (sharedPtr^.stringblkptr);
      sharedPtr^.stringblkptrtbl[sharedPtr^.curstringblock] := sharedPtr^.stringblkptr;
      end;

    { these values must change as more standard ids/types/consts are added }
    lastprocrecord := 0;
    undeftabletop := 0;
    sharedPtr^.proctabletop := 0;
    sharedPtr^.mainref := 0;

    sharedPtr^.globalsize := 0;
    sharedPtr^.ownsize := 0;
    sharedPtr^.definesize := 0;

    lastfilekey := maxint;

    anyfile := false;
    anyexternals := false;

    sharedPtr^.proctable[0].charindex := 0;
    sharedPtr^.proctable[0].opensfile := false;
    sharedPtr^.proctable[0].realfunction := false;
    sharedPtr^.proctable[0].globaldeath := false;
    sharedPtr^.proctable[0].isprocparam := false;
    sharedPtr^.proctable[0].bodydefined := false;
    sharedPtr^.proctable[0].intlevelrefs := false;
    sharedPtr^.proctable[0].externallinkage := false;
    sharedPtr^.proctable[0].referenced := true;
    sharedPtr^.proctable[0].ownused := false;
    sharedPtr^.proctable[0].calllinkage := pascal2call;
    sharedPtr^.proctable[0].registerfunction := 0;
    sharedPtr^.proctable[0].backlink := 0;
    sharedPtr^.proctable[0].charlen := 0;
    sharedPtr^.proctable[0].levelspread := 0;
    sharedPtr^.proctable[0].level := 1;

    lastid := 0;
    lastscope := 0;

    tabletop := 0;
    enterblock(0, 0, 0);
    level := 0;
    display[0].oldtabletop := 0;

    if sharedPtr^.switcheverplus[doublereals] then
      sharedPtr^.targetrealsize := doublesize
    else
      sharedPtr^.targetrealsize := singlesize;

    if sharedPtr^.switcheverplus[shortintegers] then
      begin
      sharedPtr^.targetintsize := shorttargetintsize;
      sharedPtr^.targetmaxint := shortmaxint;
      sharedPtr^.targetminint := shortminint;
      end
    else
      begin
      sharedPtr^.targetintsize := defaulttargetintsize;
      sharedPtr^.targetmaxint := defaulttargetmaxint;
      sharedPtr^.targetminint := defaulttargetminint;
      end;

    { fake entry for bad boundid's, will be overwritten by next call }
    setId (integerid, boundid, 0, nullboundindex);
    with display[displaytop] do
      namesdeclared := namesdeclared - 1;

    {<<<  define 'integer'}
    enterform (ints, f, fptr);
    with fptr^ do
      begin
      intindex := f;
      size := sharedPtr^.targetintsize;
      align := intalign;
      end;

    setId (integerid, typename, sharedPtr^.targetintsize, intindex);
    {>>>}
    {<<<  define 'shortint' subrange}
    enterform (subranges, f, fptr);
    shortintindex := f;
    with fptr^ do
      begin
      size := shorttargetintsize;
      align := shortintalign;
      lowerord := shortminint;
      upperord := shortmaxint;
      parentform := ints;
      parenttype := intindex;
      end;

    setId (shortintid, typename, shorttargetintsize, shortintindex);
    {>>>}
    {<<<  define dummy subrange for set building operations}
    enterform (subranges, f, fptr);
    with fptr^ do
      begin
      subrangeindex := f;
      size := sharedPtr^.targetintsize;
      align := intalign;
      lowerord := 0;
      upperord := maxsetord;
      parentform := ints;
      parenttype := intindex;
      parentform := ints;
      end;
    {>>>}
    {<<<  define 'real'}
    enterform (reals, f, fptr);
    with fptr^ do
      begin
      realindex := f;
      size := sharedPtr^.targetrealsize;
      align := realalign;
      end;

    setId (realid, typename, sharedPtr^.targetrealsize, realindex);
    {>>>}
    {<<<  define 'double'}
    enterform (doubles, f, fptr);
    with fptr^ do
      begin
      doubleindex := f;
      size := doublesize;
      align := realalign;
      end;

    if sharedPtr^.switcheverplus[doublereals] then
      doubleindex := realindex;

    setId (doubleid, typename, doublesize, doubleindex);
    {>>>}

    {<<<  define 'char'}
    enterform (chars, f, fptr);
    chartypeindex := f;
    with fptr^ do
      begin
      size := charsize;
      align := charalign;
      end;

    setId (charid, typename, charsize, chartypeindex);
    {>>>}
    {<<<  define 'boolean'}
    enterform (bools, f, fptr);
    with fptr^ do
      begin
      boolindex := f;
      size := scalarsize;
      align := scalaralign;
      end;

    setId (booleanid, typename, scalarsize, boolindex);
    {>>>}
    {<<<  define 'text'}
    enterform (files, f, fptr);
    with fptr^ do
      begin
      containsfile := true;
      filebasetype := chartypeindex;
      textindex := f;
      size := sharedPtr^.ptrsize;
      align := ptralign;
      filekey := maxint;
      end;

    setId (textid, typename, sharedPtr^.ptrsize, textindex);
    {>>>}

    {<<<  define 'maxint'}
    setId (maxintid, constname, 0, intindex);
    p^.constvalue.representation := ints;
    p^.constvalue.intvalue := sharedPtr^.targetmaxint;
    p^.constvalue.negated := false;
    {>>>}
    {<<<  define 'minint'}
    setId (minintid, constname, 0, intindex);
    p^.constvalue.representation := ints;
    p^.constvalue.intvalue := sharedPtr^.targetminint;
    p^.constvalue.negated := true;
    {>>>}

    {<<<  define 'true'}
    setId (trueid, constname, 0, boolindex);
    p^.constvalue.representation := ints;
    p^.constvalue.intvalue := ord(true);
    p^.constvalue.negated := false;
    {>>>}
    {<<<  define 'false'}
    setId (falseid, constname, 0, boolindex);
    p^.constvalue.representation := ints;
    p^.constvalue.intvalue := ord(false);
    p^.constvalue.negated := false;
    {>>>}

    setId (writeid, standardproc, 0, 0);
    setId (writelnid, standardproc, 0, 0);
    setId (facosid, standardfunc, 0, 0);
    setId (fasinid, standardfunc, 0, 0);
    setId (fatanid, standardfunc, 0, 0);
    setId (fatanhid, standardfunc, 0, 0);
    setId (fcoshid, standardfunc, 0, 0);
    setId (fetoxm1id, standardfunc, 0, 0);
    setId (fgetexpid, standardfunc, 0, 0);
    setId (fgetmanid, standardfunc, 0, 0);
    setId (fintid, standardfunc, 0, 0);
    setId (flog10id, standardfunc, 0, 0);
    setId (flog2id, standardfunc, 0, 0);
    setId (flognp1id, standardfunc, 0, 0);
    setId (fmodid, standardfunc, 0, 0);
    setId (fremid, standardfunc, 0, 0);
    setId (fscaleid, standardfunc, 0, 0);
    setId (fsgldivid, standardfunc, 0, 0);
    setId (fsglmulid, standardfunc, 0, 0);
    setId (fsinhid, standardfunc, 0, 0);
    setId (ftanid, standardfunc, 0, 0);
    setId (ftanhid, standardfunc, 0, 0);
    setId (ftentoxid, standardfunc, 0, 0);
    setId (ftwotoxid, standardfunc, 0, 0);
    setId (fmovecrid, standardfunc, 0, 0);
    setId (readfpcrid, standardfunc, 0, 0);
    setId (snglid, standardfunc, 0, 0);
    setId (dblid, standardfunc, 0, 0);
    setId (sinid, standardfunc, 0, 0);
    setId (cosid, standardfunc, 0, 0);
    setId (expid, standardfunc, 0, 0);
    setId (sqrtid, standardfunc, 0, 0);
    setId (arctanid, standardfunc, 0, 0);
    setId (lnid, standardfunc, 0, 0);
    setId (oddid, standardfunc, 0, 0);
    setId (absid, standardfunc, 0, 0);
    setId (sqrid, standardfunc, 0, 0);
    setId (truncid, standardfunc, 0, 0);
    setId (roundid, standardfunc, 0, 0);
    setId (ordid, standardfunc, 0, 0);
    setId (chrid, standardfunc, 0, 0);
    setId (succid, standardfunc, 0, 0);
    setId (predid, standardfunc, 0, 0);
    setId (eofid, standardfunc, 0, 0);
    setId (eolnid, standardfunc, 0, 0);
    setId (timeid, standardfunc, 0, 0);
    setId (sizeid, standardfunc, 0, 0);
    setId (bitsizeid, standardfunc, 0, 0);
    setId (upperid, standardfunc, 0, 0);
    setId (lowerid, standardfunc, 0, 0);
    setId (loopholeid, standardfunc, 0, 0);
    setId (refid, standardfunc, 0, 0);
    setId (noioerrorid, standardproc, 0, 0);
    setId (ioerrorid, standardfunc, 0, 0);
    setId (iostatusid, standardfunc, 0, 0);
    setId (copyid, standardfunc, 0, 0);
    setId (concatid, standardfunc, 0, 0);
    setId (lengthid, standardfunc, 0, 0);
    setId (posid, standardfunc, 0, 0);
    setId (seekid, standardproc, 0, 0);
    setId (readid, standardproc, 0, 0);
    setId (readlnid, standardproc, 0, 0);
    setId (breakid, standardproc, 0, 0);
    setId (newid, standardproc, 0, 0);
    setId (disposeid, standardproc, 0, 0);
    setId (packid, standardproc, 0, 9);
    setId (unpackid, standardproc, 0, 0);
    setId (putid, standardproc, 0, 0);
    setId (pageid, standardproc, 0, 0);
    setId (getid, standardproc, 0, 0);
    setId (resetid, standardproc, 0, 0);
    setId (rewriteid, standardproc, 0, 0);
    setId (closeid, standardproc, 0, 0);
    setId (deleteid, standardproc, 0, 0);
    setId (renameid, standardproc, 0, 0);
    setId (insertid, standardproc, 0, 0);
    setId (strid, standardproc, 0, 0);
    setId (valprocid, standardproc, 0, 0);
    setId (deletestrid, standardproc, 0, 0);
    setId (fsincosid, standardproc, 0, 0);
    setId (setfpcrid, standardproc, 0, 0);
    setId (forwardid, directivename, 0, 0);
    setId (externalid, directivename, 0, 0);
    setId (nonpascalid, directivename, 0, 0);
    setId (interruptid, directivename, 0, 0);

    {<<<  define noneindex for undef typenames}
    enterform (none, f, fptr);
    with fptr^ do
      begin
      dbgsymbol := 0;
      noneindex := f;
      size := 0;
      align := unitsize;
      end;
    {>>>}
    {<<<  define nilindex}
    enterform(ptrs, f, fptr);
    with fptr^ do
      begin
      ptrtypename := 0;
      nilindex := f;
      size := sharedPtr^.ptrsize;
      align := ptralign;
      end;

    nilvalue.operandkind := constoperand;
    nilvalue.cvalue.representation := ptrs;

    if reversebytes then
      begin
      kludge.p := niladdressvalue;
      for i := 1 to hostintsize div 2 do
        begin
        b := kludge.b[i];
        kludge.b[i] := kludge.b[hostintsize + 1 - i];
        kludge.b[hostintsize + 1 - i] := b;
        end;
      nilvalue.cvalue.ptrvalue := kludge.p;
      end
    else
      nilvalue.cvalue.ptrvalue := niladdressvalue;

    nilvalue.oprndlen := sharedPtr^.ptrsize;
    nilvalue.typeindex := nilindex;
    {>>>}
    {<<<  define 'nil' for debugger}
    tabletop := tabletop + 1;
    p := ref(bigtable[tabletop]);
    with p^ do
      begin
      form := false;
      name := 0;
      nextname := 0;
      namekind := constname;
      consttype := nilindex;
      constvalue := nilvalue.cvalue;
      end;
    {>>>}
    {<<<  define 'main' program}
    p := ref(bigtable[0]);
    with p^ do
      begin
      name := 0;
      form := false;
      nextname := 0;
      namekind := procname;
      charindex := 1;
      charlen := 0;
      paramlist := 0;
      functype := noneindex;
      funclen := 0;
      dbgsymbol := 0;
      end;
    {>>>}

    enterblock(1, 0, 0);
    display[1].blocksize := display[0].blocksize;
    display[1].oldtabletop := tabletop;
    level := 1;

    {<<<  define 'output'}
    setId (outputid, varname, sharedPtr^.ptrsize, textindex);
    outputindex := tabletop;
    outputdeclared := false;
    {>>>}
    {<<<  define 'input'}
    sharedPtr^.inputoffset := display[level].blocksize;
    setId (inputid, varname, sharedPtr^.ptrsize, textindex);
    inputindex := tabletop;
    {>>>}

    { Save the number of bytes allocated for input and output. }
    sharedPtr^.globalfiles := display[1].blocksize;
    sharedPtr^.consttablelimit := sharedPtr^.stringtablelimit;
    stringfilebase := sharedPtr^.stringtablelimit; {save for writeenvirfile}
  end;
  {>>>}

begin
  sharedPtr := getSharedPtr;
  tokenSharedPtr := getTokenSharedPtr;
  interSharedPtr := getInterSharedPtr;

  { the next lines are superfluous: initanalys does the work }
  sharedPtr^.stringblkptr := sharedPtr^.stringblkptrtbl[1];
  if sharedPtr^.stringblkptr = nil then
    begin
    new (sharedPtr^.stringblkptr);
    sharedPtr^.stringblkptrtbl[1] := sharedPtr^.stringblkptr;
    end;

  init;

  if token = programsym then
    programheading
  else if sharedPtr^.switcheverplus[mainbody] then
    warnnonstandard (progexpected);

  repeat
    block;

    while token = endsym do
      {<<<  extra end warning}
      begin
      warn (extraenderr);
      gettoken;
      end;
      {>>>}

    case token of
      proceduresym,
      functionsym:
        warn (extraprocerr);
      {<<<}
      dot:
        begin
        gettoken;
        verify1 ([eofsym], garbageerr)
        end;
      {>>>}
      {<<<}
      semicolon:
        begin
        warn (doteoferr);
        gettoken
        end;
      {>>>}
      {<<<}
      eofsym:
        if sharedPtr^.switchcounters[mainbody] > 0 then
          warnbetween (doteoferr);
      {>>>}
      otherwise
        analysFatal (extrastmterr);
      end;

    if token <> eofsym then
      enterblock (1, 0, 0);
    until token = eofsym;

  if intstate = opstate then
    genop (endexpr);
  genstmt (endall);

  exitblock (0);

  if emitflag then
    put (interSharedPtr^.interFile);

  dispose (labelflag);
end;
{>>>}
