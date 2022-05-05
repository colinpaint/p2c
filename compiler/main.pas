{<<<}
{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from
  Oregon Software does not apply to user-modified programs.  All copies
  of this program must display this notice and all copyright notices.
}
{$noindex} { subscript error in warnat }
{>>>}
{<<<  includes}
%include 'common.def';

%include 'token.def';
%include 'inter.def';
%include 'pseudo.def';

%include 'debug.def';
%include 'list.def';

%include 'scan.def';
%include 'analysis.def';
%include 'traverse.def';
%include 'code.def';

%include 'main.def';
{>>>}
label 99;
{<<<}
const
  qualifierlength = 12; {max length of switches ('structstatic')}
  maxswitchval = 127; {largest switch value}
{>>>}
{<<<}
type
  {<<<}
  quals = (blanks, bstepq, byteallocq, caseq, codeconstq, commonvarsq,
           cpu8086q, cpu80286q, checkq, debugq, defineq, detailsq,
           doubleq, editlistq, eisq, environq, errorsq, fisq, floatsafeq,
           fppq, framepointerq, genmaskq, groupownq, includelistq,
           largemodelq, level0q, librequestq, listq, longlibq, macroq,
           mainq, cpu68000q, cpu68020q, fpc68881q, objectq, oldpackingq,
           oldreswordsq, ownq, pascal1q, pdp11q, picq, profileq,
           sharecodeq, shortintsq, simq, standardq, statisticsq, stmtnumq,
           symbolq, tblockq, testq, timesq, truncateq, tswitch0q,
           tswitch1q, tswitch2q, tswitch3q, unixtargetq, usebsd42libq,
           usesysVlibq, versionq, walkbackq, windowsq, workspq, cplusplusq,
           ansiq, enumintsq, expandfloatq, indexcheckq, compatq,
           listincq, listexpq, modstringsq, nilcheckq, outputprepq, ppdefq,
           ppundefq, rangecheckq, romconstsq, signedcharq, structstaticq,
           vectextrefq, vectextdefq, structstandq, notfound);
  {>>>}
  qualifier = packed array [1..qualifierlength] of char; {qual name}
  qualtable = array [quals] of qualifier; {look-up table}

  {<<<}
  csiErrorName = (ambig, ambig_option, unknown, nono, qualtwice, twofilenames,
                  badfile, nofile, noinput, manyouts, badparam, numrange,
                  contradiction, badsyntax, missingparen, outconflict,
                  cmdtoolong, pic_own_section);
  {>>>}

  qualtrans = array [quals] of switch; {translate quals to switches}
  qualset = set of quals;
{>>>}
{<<<}
var
  sharedPtr: sharedPtrType;
  tokenSharedPtr: tokenSharedPtrType;

  day, month, year: Integer;

  qualindex: qualtable; { qualifier lookup table}
  numquals: qualset;    { quals with numeric parameters}
  cmdquals: qualset;    { quals set on command line}
  qualsset: qualset;    { quals turned on at start of compile}

  next: cmdindex; { next char in command string }

  filefound: boolean; { file found in this field}
  filestart, filefinish: cmdindex; { start/end of last file name found}
  fieldsfound: 0..cmdlinelength; { number of fields found}

  firstfound: boolean;  { first file field found}
  secondfound: boolean; { second file field found}
  manyfound: boolean;   { more than 2 file fields found}

  outspeced: boolean;   { the <output>=<input> form was used}

  emptyfileflag: boolean; { empty file name found, possibly an error}
  emptystart: cmdindex;   { start of empty file field}
  emptyend: cmdindex;     { end of empty file field}

  lastfield: cmdindex;    { end of last field scanned}

  fppspecified: boolean;  { if ever specified}
{>>>}

{ external utils }
{<<<}
function min;

begin
  if i < j then
    min := i
  else
    min := j
end;
{>>>}
{<<<}
function max;

begin
  if i > j then
    max := i
  else
    max := j
end;
{>>>}
{<<<}
procedure warnat (error: warning; line: integer; column: columnindex);
{ put an error message into errortable.
  Halt compilation if this is a fatal error, and force fatal error if errortable is filled by this error }

begin
  Writeln ('warnat ', error, ' line:', line:3, ' column:', column:3);

  if sharedPtr^.lasterror < errortablesize - 1 then
    { enough room, add next table entry }
    sharedPtr^.lasterror := sharedPtr^.lasterror + 1
  else
    begin
    { Too many errors, bomb out}
    sharedPtr^.fatalflag := true;
    column := 0;
    error := toomanyerrors;
    sharedPtr^.lasterror := errortablesize
    end;

  with sharedPtr^.errortable[sharedPtr^.lasterror] do
    begin
    err := error;
    errline := line;
    errcolumn := column
    end;

  if sharedPtr^.fatalflag then
    begin
    { fatal error }
    scan2;
    goto 99;
    end;
end;
{>>>}
{<<<}
procedure error;
{ Special error handler for Pascal-2.
  The primary reason for providing our own error handler is to minimize the amount of
  useless data printed to the user if the compiler can not open a file.
  The user does not need the internal compiler address at which the error occured }

var
  i: integer;

begin
  writeln;

  write (msg: errormsglength {+ 1});
  if class = ioerror then
    begin
    write (' "');
    for i := 1 to filenamelength do
      if filename[i] > ' ' then
        write (filename[i]);
    write ('"');
    end
  else
    write (' ', userpc: - 1);
  writeln;

end;
{>>>}
{<<<}
procedure panic;
{ Panic exit from analys, code or travrs.
  Assumes current procedure reference is stored in 'blockref', and that string file is still open }

var
  i: integer;
  nextstringfile: 0..diskbufsize; { index into buffer }
  nextstringblock: 0..maxint;     { block in the string file }
  stringindex: integer;           { index into stringfile }

begin
  case sharedPtr^.abortmsg of
    wrongversionenv: write ('Recompile environment file with this compiler. Error');
    outofmem:        write ('Out of memory');
    undeltemps:      write ('Undeleted temps');
    muchcode:        write ('Too much object code');
    manylabels:      write ('Too many labels');
    manyplabels:     write ('Too many Pascal labels');
    manytemps:       write ('Code too complex');
    manynodes:       write ('Too many nodes');
    builderror: ;
    manykeys:        write ('Too many keys');
    walkerror: ;
    interntemp:      write ('Internal temp error');
    badadjust:       write ('Bad adjustoffset value');
    inconsistent: ;
    manyexterns:     write ('Too many external references');
    badrelfile:      write ('Bad relocation file');
    manynonlocals:   write ('Too many non-locals');
    perposdump:      write ('Block too long');
    end;

  if sharedPtr^.abortmsg in [undeltemps, builderror, walkerror, interntemp, badadjust, inconsistent, badrelfile] then
    begin
    writeln;
    write ('Internal compiler error');
    end;

  if sharedPtr^.current_line <> 0 then
    write (' on line ', sharedPtr^.current_line:1,', stmt ', sharedPtr^.current_stmt:1, ',');

  if sharedPtr^.blockref = 0 then
    write (' in main program')
  else
    begin
    write (' in procedure ');
    for i := 1 to sharedPtr^.proctable[sharedPtr^.blockref].charlen do
      begin
      stringindex := i + sharedPtr^.stringfilecount + sharedPtr^.proctable[sharedPtr^.blockref].charindex - 2;
      nextstringblock := stringindex div (diskbufsize + 1) + 1;
      if sharedPtr^.stringblkptrtbl[nextstringblock] = nil then
        new (sharedPtr^.stringblkptrtbl[nextstringblock]);
      sharedPtr^.stringblkptr := sharedPtr^.stringblkptrtbl[nextstringblock];
      nextstringfile := stringindex mod (diskbufsize + 1);
      write (chr (sharedPtr^.stringblkptr^[nextstringfile]));
      end;
    end;
  writeln (' during Pascal-2 compilation');

  if not sharedPtr^.switcheverplus[test] then
    exitst (exitstatus);
end;
{>>>}
{<<<}
procedure abort (msg: abortwarning);
{ abort in a panic, printing a message and killing the compile }

begin
  sharedPtr^.abortmsg := msg;
  panic;
end;
{>>>}

{ files }
{<<<}
procedure getFileName (which: FilenameListPtr; stripdevice: boolean; stripext: boolean;
                       var result: FilenameBuf; var resultlength: FilenameIndex);
{ Get a file name with or without the device or extension fields }

var
  start, finish: FilenameIndex; {limits of file name}
  i, j: FilenameIndex;
  scanning: boolean;

begin
  if which = nil then
    begin
    which := sharedPtr^.sourceListHead;
    while which^.next <> nil do
      which := which^.next;
    stripdevice := true;
    stripext := true;
    end;

  with which^ do
    begin
    start := 1;
    finish := arglen;

    if stripdevice then
      {<<<  strip : device name}
      begin
      for i := 1 to arglen do
        if (arg[i] = ':') or (arg[i] = ']') then
          start := i + 1;
      end;
      {>>>}

    if stripext then
      {<<<  strip . extension}
      begin
      i := arglen;
      scanning := true;

      while scanning and (i >= start) do
        {don't use char sets: too much data space used}
        if not ((arg[i] = ':') or (arg[i] = ']') or (arg[i] = '.')) then
          i := i - 1
        else
          scanning := false;

      if (i >= start) and (arg[i] = '.') then
        finish := i - 1
      end;
      {>>>}

    resultlength := finish - start + 1;

    i := 0;
    for j := start to finish do
      begin
      i := i + 1;
      result[i] := arg[j];
      end;
    for i := i + 1 to filenamelen do
      result[i] := ' ';
    end;
end;
{>>>}
{<<<}
procedure getOutputName;
{ fill global "filename" and "outputname" }

var
  i: FilenameIndex;
  limit: 1..maxprocnamelen;

begin
  getFileName (nil, true, true, sharedPtr^.filename, sharedPtr^.filenameLength);
  limit := min (sharedPtr^.filenameLength, maxprocnamelen);

  for i := 1 to limit do
    if (sharedPtr^.filename[i] >= 'a') and (sharedPtr^.filename[i] <= 'z') then
      sharedPtr^.outputname[i] := chr(ord(sharedPtr^.filename[i]) - (ord('a') - ord('A')))
    else
      sharedPtr^.outputname[i] := sharedPtr^.filename[i];

  for i := limit + 1 to maxprocnamelen do
    sharedPtr^.outputname[i] := ' ';
end;
{>>>}
{<<<}
procedure openSource;
{ open commandLine source file }

begin
  getFileName (sharedPtr^.sourceListHead, false, false, sharedPtr^.filename, sharedPtr^.filenameLength);
  Writeln ('openSource level:', sharedPtr^.sourcelevel:1, ' filename:', sharedPtr^.filename);
  reset (sharedPtr^.source[sharedPtr^.sourcelevel], sharedPtr^.filename);
end;
{>>>}
{<<<}
procedure openInclude;
{ open an included source file at current nesting level, file in global variable "filename" }

begin
  Writeln ('openInclude - level:', sharedPtr^.sourcelevel:1, ' filename:', sharedPtr^.filename);
  reset (sharedPtr^.source[sharedPtr^.sourceLevel], sharedPtr^.filename);
end;
{>>>}

{ commandLine }
{<<<}
function uc (c: char): char;
{ Return the upper case equivalent of a character }

begin
  if (c >= 'a') and (c <= 'z') then
    uc := chr(ord(c) + (ord('A') - ord('a')))
  else
    uc := c;
end;
{>>>}
{<<<}
procedure skipBlanks;

begin
  while (next < sharedPtr^.cmdlength) and (sharedPtr^.cmdline[next] = ' ') do
    next := next + 1;
end;
{>>>}
{<<<}
procedure printVersion;
{ Print a version message }

begin
  writeln ('Pascal-2(tm) ', systemtitle, ' ', versionstring);
end;
{>>>}
{<<<}
procedure commandLineError (which: csiErrorName; startind: cmdindex; endind: cmdindex);
{ Print an error message and exit from the compiler with a fatal error status.
  The offending portion of the command line is printed before the error message
}
var
  i: integer;

begin
  printversion;

  for i := startind to endind do
    write (sharedPtr^.cmdline[i]);

  if startind <= endind then
    writeln;

  case which of
    ambig:           writeln ('Ambiguous switch.');
    ambig_option:    writeln ('Ambiguous switch option.');
    unknown:         writeln ('Unknown switch.');
    nono:            writeln ('"NO" not allowed on this switch.');
    qualtwice:       writeln ('Same switch used twice.');
    twofilenames:    writeln ('Two file names in one field.');
    badfile:         writeln ('Bad file name syntax.');
    nofile:          writeln ('No file in field.');
    noinput:         writeln ('No input file provided.');
    manyouts:        writeln ('More than two output file specifications.');
    badparam:        writeln ('Required parameter missing.');
    numrange:        writeln ('Value for qualifier out of range.');
    contradiction:   writeln ('Conflicting switches specified.');
    badsyntax:       writeln ('Bad command line syntax.');
    missingparen:    writeln ('Missing ")".');
    outconflict:     writeln ('Output file(s) specified both ways.');
    cmdtoolong:      writeln ('Command line too long.');
    pic_own_section: writeln ('Own section must be 15 with PIC');
    end;

  exitst (exitstatus); {value is opsys dependent}
end;
{>>>}
{<<<}
procedure parseCommandLine;

  {<<<}
  procedure getcommandline;
  { Read command line }

  var
    i: cmdindex;

  begin
    sharedPtr^.cmdlength := 1;
    P_getcmdline (sharedPtr^.cmdLine, sharedPtr^.cmdlength);

    sharedPtr^.cmdlength := min (cmdlinelength, sharedPtr^.cmdlength + 1);
    for i := sharedPtr^.cmdlength to cmdlinelength do
      sharedPtr^.cmdline[i] := ' ';
  end;
  {>>>}
  {<<<}
  procedure initquals;
  { Initialize the qualifier table. }

  var
    i: quals;

  begin
    for i := blanks to notfound do
      qualindex[i] := ' XXXXXXXXXXX';

    qualindex[checkq] := 'CHECK       ';
    qualindex[caseq] := 'CASE        ';
    qualindex[debugq] := 'DEBUG       ';
    qualindex[detailsq] := 'DETAILS     ';
    qualindex[doubleq] := 'DOUBLE      ';
    qualindex[errorsq] := 'ERRORS      ';
    qualindex[genmaskq] := 'GENMASK     ';
    qualindex[includelistq] := 'INCLUDE     ';
    qualindex[level0q] := 'LEVEL0      ';
    qualindex[listq] := 'LIST        ';
    qualindex[macroq] := 'MACRO       ';
    qualindex[mainq] := 'MAIN        ';
    qualindex[objectq] := 'OBJECT      ';
    qualindex[ownq] := 'OWN         ';
    qualindex[profileq] := 'PROFILE     ';
    qualindex[shortintsq] := 'SHORTINTS   ';
    qualindex[standardq] := 'STANDARD    ';
    qualindex[stmtnumq] := 'STMTNUM     ';
    qualindex[symbolq] := 'SYMBOLS     ';
    qualindex[tblockq] := 'TBLOCK      ';
    qualindex[testq] := 'TEST        ';
    qualindex[timesq] := 'TIMES       ';
    qualindex[tswitch0q] := 'TSWITCH0    ';
    qualindex[tswitch1q] := 'TSWITCH1    ';
    qualindex[tswitch2q] := 'TSWITCH2    ';
    qualindex[tswitch3q] := 'TSWITCH3    ';
    qualindex[versionq] := 'VERSION     ';
    qualindex[walkbackq] := 'WALKBACK    ';
    qualindex[defineq] := 'DEFINE      ';
    qualindex[environq] := 'ENVIRONMENT ';
    qualindex[framepointerq] := 'FRAMEPOINTER';
    qualindex[oldreswordsq] := 'OLDRESWORDS ';

    qualindex[cpu68000q] := '68000       ';
    qualindex[cpu68020q] := '68020       ';
    qualindex[fpc68881q] := '68881       ';
    qualindex[picq] := 'PIC         ';
    qualindex[longlibq] := 'LONGLIB     '
  end;
  {>>>}
  {<<<}
  procedure addtofilelist (var list: filenamelistptr; start, finish: cmdindex; isinclude: boolean);
  { Append a filename (from the command line) to the given list }

  var
    p: filenamelistptr; {induction on existing list}
    q: filenamelistptr; {points to new entry}
    i: filenameindex; {induction on filename}
    directorydelim: boolean; {flags a delimiter}
    dotcount: integer; {counts down vdos fields}

  begin
    directorydelim := false;

    new (q);
    with q^ do
      begin
      next := nil;
      arglen := min(finish - start, filenamelen);
      for i := 1 to filenamelen do
        begin
        if start < finish then
          begin
          arg[i] := sharedPtr^.cmdline[start];
          start := start + 1;
          end
        else
          arg[i] := ' ';
        end;
      end;

    if list <> nil then
      begin
      p := list;
      while p^.next <> nil do
        p := p^.next;
      p^.next := q;
      end
    else
      list := q;

  end;
  {>>>}
  {<<<}
  procedure takefilename (parenthesized: boolean; {it's in a parenthesized list}
                          var list: filenamelistptr; {where to store it}
                          var next: cmdindex; {index in command line}
                          isinclude: boolean {if this file in includes});
  { Parse a filename, and add it to the list specified by "list".
    This routine makes sure that there is only one file specified per field
  }

  var
    startindex: cmdindex; {start of this field}
    terminators: set of char; {what stops a file name}
    quotedstring: boolean; {flag for quote removal}

    {<<<}
    procedure skipbalancedstring;
    { Skip a string balanced with respect to parentheses and quoted strings }

      var
        endchar: char; {bracket which terminates string}

      begin {skipbalancedstring}
        if sharedPtr^.cmdline[next] in ['(', '[', '<'] then
          begin
          if sharedPtr^.cmdline[next] = '(' then
            endchar := ')'
          else if sharedPtr^.cmdline[next] = '<' then
            endchar := '>'
          else
            endchar := ']';
          next := next + 1;
          while (sharedPtr^.cmdline[next] <> endchar) and (next <> sharedPtr^.cmdlength) do
            skipbalancedstring;
          end

        else if sharedPtr^.cmdline[next] = '"' then
          begin
          quotedstring := true;
          repeat
            next := next + 1
          until (sharedPtr^.cmdline[next] = '"') or (next = sharedPtr^.cmdlength);
          end;

        if next < sharedPtr^.cmdlength then
          next := next + 1
        else
          commandLineError (badfile, startindex, sharedPtr^.cmdlength);

      end {skipbalancedstring} ;
    {>>>}

  begin
    quotedstring := false;
    startindex := next;

    terminators := [',', '/', '=', ' '];
    if parenthesized then
      terminators := terminators + [')'];

    while (next < sharedPtr^.cmdlength) and not (sharedPtr^.cmdline[next] in terminators) do
      skipbalancedstring;

    if filefound then
      commandLineError (twofilenames, filestart, next)
    else
      begin
      filefound := true;
      filestart := startindex;
      filefinish := next;
      if quotedstring then
        begin
        filestart := filestart + 1;
        if (sharedPtr^.cmdline[next] = '"') or (filefinish <= sharedPtr^.cmdlength) then
          filefinish := filefinish - 1
        end;

      addtofilelist (list, filestart, filefinish, isinclude);
      end;
  end;
  {>>>}
  {<<<}
  procedure takequal (var next: cmdindex);
  { Parse and look up a qualifier, updating "next" to point to the next character in the command line }

  var
    s: integer; {temporary}
    startingindex: cmdindex; {start of qualifier, for error printout}
    quali: 0..qualifierlength; {current character in qual being built}
    name: qualifier; {qualifier name}
    nofound: boolean; {true if "no" preceeds qualifier}
    thisqual: quals; {qualifier just found}
    ambiguous: boolean; {qualifier lookup was ambiguous}

    {<<<}
    procedure findqual (target: qualifier; {candidate Qual name}
                       var result: quals; {result of lookup}
                       var ambiguous: boolean {more than one match} );
    { Look up "target" in the Qualindex and set "result" to the appropriate
      qualifier.  If there is a full match, this is always taken.  Otherwise,
      a single partial match will be accepted.  Multiple partial matches cause
      "ambiguous" to be set.
    }
    var
      partialmatch: boolean; {partially matches the Qual so far}
      partialresult: quals; {where the match was}
      partials: 0..maxint; {counter of partial matches}
      effectivelength: 0..qualifierlength; {significant chars in target}
      i: 1..qualifierlength; {induction var}

    begin
      partials := 0;
      effectivelength := 0;
      for i := 1 to qualifierlength do
        if target[i] <> ' ' then effectivelength := i;
      result := blanks;
      qualindex[notfound] := target; {to terminate search}

      while target <> qualindex[result] do
        begin
        result := succ(result);
        partialmatch := target <> qualindex[result];
        for i := 1 to effectivelength do
          partialmatch := partialmatch and
                          (target[i] = qualindex[result, i]);
        if partialmatch then
          begin
          partialresult := result;
          partials := partials + 1;
          end;
        end;
      if (result = notfound) and (partials = 1) then
        result := partialresult;
      ambiguous := partials > 1;
    end;
    {>>>}
    {<<<}
    procedure getnumqual (var result: integer; {resulting value}
                         low_lim, hi_lim: integer {limits on value} );
    { Scan off a number, for qualifiers that take numeric arguments }

    var
      tempres: integer;
      accumulating, negate: boolean;

    begin
      if sharedPtr^.cmdline[next] in [':', '='] then
        next := next + 1
      else
        commandLineError(badparam, startingindex, next - 1);

      accumulating := true;
      tempres := 0;

      negate := sharedPtr^.cmdline[next] = '-';
      if negate then
        next := next + 1;

      while sharedPtr^.cmdline[next] in ['0'..'9'] do
        begin
        if accumulating then
          if tempres <= maxint div 10 then
            tempres := tempres * 10
          else
            accumulating := false;
        if accumulating then
          if tempres <= maxint - (ord(sharedPtr^.cmdline[next]) - ord('0')) then
            tempres := tempres + (ord(sharedPtr^.cmdline[next]) - ord('0'))
          else
            accumulating := false;
        next := next + 1;
        end;

      if negate then
        tempres := - tempres;

      if accumulating and (tempres <= hi_lim) and (tempres >= low_lim) then
        result := tempres
      else
        commandLineError (numrange, startingindex, next - 1);
    end;
    {>>>}
    {<<<}
    procedure getspecialfilename (q: quals; {which qualifier}
                                 var next: cmdindex);
    { Handle the "/<qual>=<filename>" format for the macro, object, list, errors, include and environment qualifiers,
      and the "/include=(<filenamelist>)" format }

    var
      start: cmdindex; {start of a filename}
      savefilefound: boolean; {holds filefound during takefilename}

      {<<<}
      procedure pickalist (q: quals; {which qualifier this file goes with}
                          parenthesized: boolean {in a parenthesized list} );
      { Put the filename (just found) in the appropriate list. }


        begin {pickalist}
          if q = objectq then
            takefilename (parenthesized, sharedPtr^.objname, next, false)
          else if q = macroq then
            takefilename (parenthesized, sharedPtr^.macname, next, false)
          else if (q = listq) or (q = errorsq) then
            takefilename (parenthesized, sharedPtr^.listname, next, false)
        end;
      {>>>}

    begin
      savefilefound := filefound;
      next := next + 1;

      if sharedPtr^.cmdline[next] <> '(' then
        begin
        start := next;
        filefound := false;
        pickalist (q, false);
        end

      else
        begin
        repeat
          next := next + 1;
          start := next;
          filefound := false;
          pickalist (q, true);
          skipBlanks;
          if not (q in [environq, includelistq]) and (sharedPtr^.cmdline[next] <> ')') then
            commandLineError (missingparen, start - 1, next);
        until (next >= sharedPtr^.cmdlength) or (sharedPtr^.cmdline[next] = ')');

        if next < sharedPtr^.cmdlength then
          next := next + 1
        else
          commandLineError (missingparen, start - 1, sharedPtr^.cmdlength);

        filefound := savefilefound;
        end;
    end;
    {>>>}

  begin
    repeat
      next := next + 1;
    until (next >= sharedPtr^.cmdlength) or (sharedPtr^.cmdline[next] <> ' ');

    startingindex := next;
    quali := 0;
    nofound := false;
    if (next < cmdlinelength - 2) then
      if (uc(sharedPtr^.cmdline[next]) = 'N') and (uc(sharedPtr^.cmdline[next + 1]) = 'O') then
        begin
        nofound := true;
        next := next + 2;
        end;

    while (next <= cmdlinelength) and (sharedPtr^.cmdline[next] in ['A'..'Z', 'a'..'z', '0'..'9']) do
      begin
      if quali < qualifierlength then
        begin
        quali := quali + 1;
        name[quali] := uc(sharedPtr^.cmdline[next]);
        end;
      next := next + 1;
      end;

    if quali = 0 then
      commandLineError (badsyntax, 2, 1);

    while quali < qualifierlength do
      begin
      quali := quali + 1;
      name[quali] := ' ';
      end;

    findqual (name, thisqual, ambiguous);

    if ambiguous then
      commandLineError (ambig, startingindex, next - 1)
    else if thisqual = notfound then
      commandLineError (unknown, startingindex, next - 1);

    if nofound and (thisqual in [cpu68000q, cpu68020q, fpc68881q, usebsd42libq, usesysVlibq]) then
      commandLineError (nono, startingindex, next - 1);

    if thisqual in numquals then
      begin
      if nofound then
        commandLineError (nono, startingindex, next - 1);
      case thisqual of
        ownq:
          begin
          getnumqual (s, 0, 15);
          sharedPtr^.datasection := s;
          end;
        tblockq:
          getnumqual (sharedPtr^.tblocknum, 0, maxint);
        genmaskq:
          getnumqual (sharedPtr^.genoptmask, -65535, 65535);
        end;
      end;

    if thisqual in cmdquals then
      commandLineError (qualtwice, startingindex, next - 1);

    if not (thisqual in [environq, includelistq]) then {more than one of these is ok}
      cmdquals := cmdquals + [thisqual];

    if nofound then
      qualsset := qualsset - [thisqual]
    else
      qualsset := qualsset + [thisqual];

    { Handle switch-specified file names }
    if (thisqual in [environq, errorsq, includelistq, listq, macroq, objectq, defineq]) and
       (next <= cmdlinelength) and (sharedPtr^.cmdline[next] in [':', '=']) then
      if nofound then
        commandLineError (nono, startingindex, next)
      else
        getspecialfilename (thisqual, next);
    end;
  {>>>}
  {<<<}
  procedure endfield (var next: cmdindex);
  { Terminate a field, making several checks. }

  begin
    fieldsfound := fieldsfound + 1;

    if (fieldsfound = 1) and filefound then
      firstfound := true
    else if (fieldsfound = 2) and filefound then
      secondfound := true
    else if filefound then
      manyfound := true;

    if not filefound then
      if outspeced then
        commandLineError (nofile, lastfield, next)
      else if not emptyfileflag then
        begin
        emptyfileflag := true;
        emptystart := lastfield;
        emptyend := next;
        end;

    if sharedPtr^.cmdline[next] = '=' then
      if fieldsfound > 2 then
        commandLineError (manyouts, 1, next - 1)
      else
        begin
        fieldsfound := 2;
        outspeced := true;
        end;
    lastfield := next;

    if next < sharedPtr^.cmdlength then
      next := next + 1;

    filefound := false;
  end;
  {>>>}
  {<<<}
  function twoof (mask: qualset): boolean;
  { True if there are two or more entries in qualsset matching the mask }

  var
    q: quals;
    count: integer;  { qualifiers found in the set }
    mquals: qualset; { masked qualifier set }

  begin
    mquals := qualsset * mask;

    count := 0;
    for q := blanks to notfound do if q in mquals then
      count := count + 1;

    twoof := count > 1;
  end;
  {>>>}
  {<<<}
  procedure checkConsistency;
  { Check the command line and qualifiers for consistency. }

  var
    thisopt: quals; {current hardware options}

  begin
    if not ((outspeced and manyfound) or firstfound) then
      commandLineError (noinput, 2, 1);

    if not outspeced and emptyfileflag then
      commandLineError (nofile, emptystart, emptyend);

    if outspeced then
      if firstfound and ((sharedPtr^.objname <> nil) and (sharedPtr^.macname <> nil) or
         (sharedPtr^.objname <> nil) and not (macroq in qualsset) or
         (sharedPtr^.macname <> nil) and not (objectq in qualsset)) or
         secondfound and (sharedPtr^.listname <> nil) then
        commandLineError (outconflict, 2, 1);

    if (picq in qualsset) and (ownq in qualsset) and
       (sharedPtr^.datasection <> defdatasection) then
      commandLineError (pic_own_section, 2, 1);

    if twoof ([listq, errorsq]) or twoof([defineq, objectq, macroq]) or
       twoof ([errorsq, debugq, profileq]) or
       twoof ([cpu68000q, cpu68020q]) or
       twoof ([cpu68000q, fpc68881q]) or
       (([debugq, profileq] * qualsset <> []) and
       not (mainq in qualsset)) then
      commandLineError (contradiction, 2, 1);

    if defineq in qualsset then
      qualsset := qualsset - [objectq, macroq, mainq]
    else if ((outspeced and firstfound) or not outspeced) and (qualsset * [objectq, macroq] = []) then
      qualsset := qualsset + ([objectq] - cmdquals);

    if qualsset * [listq, errorsq] = [] then
      if outspeced and secondfound then
        qualsset := qualsset + [listq]
      else if (qualsset * [debugq, profileq] = []) then
        begin
        sharedPtr^.forceList := true;
        qualsset := qualsset + [errorsq]
        end;

    if qualsset * [debugq, profileq] <> [] then
      qualsset := qualsset + [listq, symbolq] - [walkbackq];

    if qualsset * [debugq, profileq, walkbackq, sharecodeq] <> [] then
      qualsset := qualsset + [framepointerq];

    if symbolq in qualsset then
      if qualsset * [objectq, macroq, defineq] = [] then
        commandLineError (contradiction, 2, 1);

    if twoof([standardq, caseq]) then
      commandLineError (contradiction, 2, 1);
  end;
  {>>>}
  {<<<}
  procedure passqQalsToCompiler;
  { Pass the command options from qualsset to the compiler }

  var
    i: quals;
    j: switch;
    trans: qualtrans;

  begin
    trans[cplusplusq] := cplusplus;
    trans[ansiq] := noswitch;
    trans[compatq] := compatibility;
    trans[enumintsq] := enumints;
    trans[expandfloatq] := expandfloat;
    trans[indexcheckq] := indexcheck;
    trans[listincq] := listincludes;
    trans[listexpq] := listexpansion;
    trans[modstringsq] := modstrings;
    trans[nilcheckq] := nilcheck;
    trans[outputprepq] := outputprep;
    trans[ppdefq] := noswitch;
    trans[ppundefq] := noswitch;
    trans[romconstsq] := romconsts;
    trans[signedcharq] := signedchars;
    trans[structstaticq] := structstatic;
    trans[vectextrefq] := vectextref;
    trans[vectextdefq] := vectextdef;

    trans[blanks] := noswitch;
    trans[bstepq] := bstep;
    trans[byteallocq] := bytealloc;
    trans[caseq] := caseswitch;
    trans[codeconstq] := codeconst;
    trans[checkq] := rangecheck;
    trans[commonvarsq] := commonvars;
    trans[cpu68000q] := cpu68000;
    trans[cpu68020q] := cpu68020;
    trans[cpu8086q] := cpu8086;
    trans[cpu80286q] := cpu80286;
    trans[debugq] := debugging;
    trans[defineq] := defineswitch;
    trans[detailsq] := details;
    trans[doubleq] := doublereals;
    trans[editlistq] := editlist;
    trans[eisq] := eis;
    trans[environq] := environswitch;
    trans[errorsq] := listerrors;
    trans[fisq] := fis;
    trans[floatsafeq] := floatsafe;
    trans[fpc68881q] := fpc68881;
    trans[fppq] := fpp;
    trans[framepointerq] := framepointer;
    trans[genmaskq] := genmask;
    trans[groupownq] := groupown;
    trans[includelistq] := noswitch;
    trans[largemodelq] := largemodel;
    trans[level0q] := level0;
    trans[librequestq] := librequest;
    trans[listq] := listcount;
    trans[longlibq] := longlib;
    trans[macroq] := outputmacro;
    trans[mainq] := mainbody;
    trans[objectq] := outputobj;
    trans[oldpackingq] := oldpacking;
    trans[oldreswordsq] := oldreswords;
    trans[ownq] := own;
    trans[pascal1q] := pascal1;
    trans[pdp11q] := pdp11data;
    trans[picq] := pic;
    trans[profileq] := profiling;
    trans[sharecodeq] := sharecode;
    trans[shortintsq] := shortintegers;
    trans[simq] := sim;
    trans[standardq] := standard;
    trans[statisticsq] := statistics;
    trans[stmtnumq] := stmtnumbers;
    trans[symbolq] := symboltable;
    trans[tblockq] := tblock;
    trans[testq] := test;
    trans[timesq] := timing;
    trans[truncateq] := truncatesw;
    trans[tswitch0q] := tswitch0;
    trans[tswitch1q] := tswitch1;
    trans[tswitch2q] := tswitch2;
    trans[tswitch3q] := tswitch3;
    trans[usebsd42libq] := usebsd42lib;
    trans[usesysVlibq] := usesysVlib;
    trans[versionq] := noswitch;
    trans[walkbackq] := walkback;
    trans[windowsq] := windows;
    trans[workspq] := noswitch;
    trans[notfound] := noswitch;

    for j := noswitch to finalswitch do
      begin
      sharedPtr^.switchcounters[j] := 0;
      sharedPtr^.switcheverplus[j] := false;
      end;

    for i := blanks to notfound do
      if i in qualsset then
        begin
        sharedPtr^.switchcounters[trans[i]] := 1;
        sharedPtr^.switcheverplus[trans[i]] := true;
        if i = checkq then
          begin
          sharedPtr^.switchcounters[indexcheck] := 1;
          sharedPtr^.switcheverplus[indexcheck] := true;
          sharedPtr^.switchcounters[mathcheck] := 1;
          sharedPtr^.switcheverplus[mathcheck] := true;
          sharedPtr^.switchcounters[nilcheck] := 1;
          sharedPtr^.switcheverplus[nilcheck] := true;
          sharedPtr^.switchcounters[rangecheck] := 1;
          sharedPtr^.switcheverplus[rangecheck] := true;
          sharedPtr^.switchcounters[stackcheck] := 1;
          sharedPtr^.switcheverplus[stackcheck] := true;
          end;

        { !!!! is this right for 68020 !!! }
        if i = fpc68881q then
          begin
          sharedPtr^.switcheverplus[cpu68000] := false;
          sharedPtr^.switcheverplus[cpu68020] := true;
          end;
        end;
  end;
  {>>>}

begin
  sharedPtr := getSharedPtr;

  sharedPtr^.targetintsize := defaulttargetintsize;
  sharedPtr^.targetrealsize := defaulttargetrealsize;
  sharedPtr^.targetmaxint := defaulttargetmaxint;
  sharedPtr^.ptrsize := defaultptrsize;
  sharedPtr^.returnlinksize := defreturnlinksize;
  sharedPtr^.extreturnlinksize := defextreturnlinksize;

  initquals;

  next := 1;
  fieldsfound := 0;
  lastfield := 1;
  filefound := false;
  outspeced := false;
  emptyfileflag := false;
  firstfound := false;
  secondfound := false;
  manyfound := false;

  numquals := [workspq, tblockq, genmaskq];
  cmdquals := [];
  qualsset := [checkq, mainq, walkbackq, framepointerq, librequestq];
  fppspecified := false;

  getcommandline;
  repeat
    skipBlanks;
    case sharedPtr^.cmdline[next] of
      '/':
        takequal (next);
      ' ': ;
      otherwise
        takefilename (false, sharedPtr^.sourceListHead, next, false);
      end;
    until next >= sharedPtr^.cmdlength;

  endfield (next);

  if not outspeced then
    fieldsfound := fieldsfound + 2; {fake fields}

  checkConsistency;
  printVersion;

  if not (framepointerq in qualsset) then
    begin
    sharedPtr^.returnlinksize := sharedPtr^.ptrsize;
    sharedPtr^.extreturnlinksize := sharedPtr^.ptrsize;
    end;

  passqQalsToCompiler;

  if sharedPtr^.switcheverplus[doublereals] then
    sharedPtr^.targetrealsize := doublesize;

  if sharedPtr^.switcheverplus[shortintegers] then
    begin
    sharedPtr^.targetintsize := shorttargetintsize;
    sharedPtr^.targetmaxint := shortmaxint;
    end;
end;
{>>>}

{<<<}
procedure resetswitches;
{ reset switch table between passes }

begin
  sharedPtr^.switchcounters := sharedPtr^.originalswitches;
  sharedPtr^.currentswitch := 1;

  sharedPtr^.putlow := 0;
  sharedPtr^.puthi := 0;
  sharedPtr^.getlow := 0;
  sharedPtr^.gethi := 0;
end;
{>>>}
{<<<}
procedure setoptions;
{ set overall options based on genmask and command line }

var
  genitem: gentypes; { quasi induction for handling codegen mask }
  mask: unsignedint; { mask for handling codegen optimization options }
  maskbit: integer; { induction for checking optimization mask bits }
  disabled: boolean; { true if mask specifies disabling }

begin
  { set default code generation options set }
  sharedPtr^.genset := [firstgenopt..lastgenopt];

  if sharedPtr^.genoptmask < 0 then
    begin
    mask := abs(sharedPtr^.genoptmask);
    disabled := false;
    end
  else
    begin
    disabled := true;
    mask := sharedPtr^.genoptmask;
    end;

  { now set the options based upon -debug -profile }
  if sharedPtr^.switcheverplus[debugging] or sharedPtr^.switcheverplus[profiling] then
    sharedPtr^.genset := sharedPtr^.genset - [lifetimes, propagation, hoisting, removedeadcode,
                                              subexpressions, tailmerging, bitops];

  { if a codegen options mask was specified, update the set }
  if sharedPtr^.switcheverplus[genmask] then
    begin
    if sharedPtr^.switcheverplus[test] then
      begin
      if disabled then
        write ('DISABLED: ')
      else
        write ('ENABLED: ');
      end;

    genitem := firstgenopt;

    { walk through the mask }
    sharedPtr^.overrideset := [];
    for maskbit := 0 to 15 do
      begin
      if odd (mask) then
        begin
        sharedPtr^.overrideset := sharedPtr^.overrideset + [genitem];
        if sharedPtr^.switcheverplus[test] then
          write (maskbit: 3);
        end;

      mask := mask div 2;
      genitem := succ (genitem);
      end;

    if disabled then
      sharedPtr^.genset := sharedPtr^.genset - sharedPtr^.overrideset
    else
      sharedPtr^.genset := sharedPtr^.genset + sharedPtr^.overrideset;

    if sharedPtr^.switcheverplus[test] then
      writeln;
    end;
end;
{>>>}
{<<<}
procedure settime;

var
  day, month, year: integer;

begin
  TimeStamp (day, month, year, sharedPtr^.istarthour, sharedPtr^.istartmin, sharedPtr^.istartsec);
end;
{>>>}
{<<<}
procedure printtime (pass: packed array [l..h: shortint] of char);

var
  day, month, year, deltasec, deltamin, deltahour: integer;
  i: integer;

begin
  TimeStamp (day, month, year, deltahour, deltamin, deltasec);

  if deltahour < sharedPtr^.istarthour then
    deltahour := deltahour + 24;
  deltasec := max (deltasec - sharedPtr^.istartsec +
              60 * ((deltamin - sharedPtr^.istartmin) + 60 * (deltahour - sharedPtr^.istarthour)), 1);

  for i := l to h do
    write (pass[i]);
  writeln (' took ', deltasec: 1, ' sec, ',
           sharedPtr^.lastLine: 1, ' lines, ',
           (sharedPtr^.lastLine * 60) div deltasec: 1, ' lines/min');
end;
{>>>}

{ main program }
begin
  initShared;
  sharedPtr = getSharedPtr;
  tokenSharedPtr = getTokenSharedPtr;

  {<<<  command line}
  parseCommandLine;
  setoptions;
  sharedPtr^.originalswitches := sharedPtr^.switchcounters;
  {>>>}

  TimeStamp (day, month, year, sharedPtr^.starthour, sharedPtr^.startmin, sharedPtr^.startsec);
  settime;
  {<<<  scan/analysis}
  rewrite (getSharedPtr^.localFile, 'output.local');
  rewrite (getTokenSharedPtr^.tokenFile, 'output.token');
  rewrite (getInterSharedPtr^.interFile, 'output.inter');

  scan1;
  analys;
  scan2;

  close (getSharedPtr^.localFile);
  close (getTokenSharedPtr^.tokenFile);
  close (getInterSharedPtr^.interFile);

  printtime ('scan/analysis');
  {>>>}

  if (sharedPtr^.lasterror = 0) and
     (sharedPtr^.switcheverplus[outputmacro] or sharedPtr^.switcheverplus[outputobj]) then
    {<<<  traverse/code}
    begin
    resetswitches;
    settime;

    reset (getSharedPtr^.localFile, 'output.local');
    reset (getInterSharedPtr^.interFile, 'output.inter');
    rewrite (getPseudoSharedPtr^.pseudoFile, 'output.pseudo');

    if travcode then { traverse/code together }
      begin
      initCode;
      traverse;
      exitCode;

      close (getSharedPtr^.localFile);
      close (getInterSharedPtr^.interFile);
      close (getPseudoSharedPtr^.pseudoFile);

      printtime ('traverse/code');
      end

    else {  traverse then code}
      begin
      traverse;

      close (getSharedPtr^.localFile);
      close (getInterSharedPtr^.interFile);
      close (getPseudoSharedPtr^.pseudoFile);
      printtime ('traverse');

      resetswitches;
      settime;

      reset (getPseudoSharedPtr^.pseudoFile, 'output.pseudo');
      code;
      close (getPseudoSharedPtr^.pseudoFile);

      printtime ('code');
      end;

    if sharedPtr^.switcheverplus[symboltable] then
      closed;

    { stats }
    writeln (sharedPtr^.proctabletop:1, ' procedures, ', sharedPtr^.insertions:1, ' identifiers');

    { timing }
    TimeStamp (day, month, year, sharedPtr^.endhour, sharedPtr^.endmin, sharedPtr^.endsec);
    if sharedPtr^.endhour < sharedPtr^.starthour then
      sharedPtr^.endhour := sharedPtr^.endhour + 24;
    sharedPtr^.endsec := max (sharedPtr^.endsec - sharedPtr^.startsec +
                              60 * ((sharedPtr^.endmin - sharedPtr^.startmin) +
                                    60 * (sharedPtr^.endhour - sharedPtr^.starthour)), 1);

    writeln ('took ', sharedPtr^.endsec:1, 'sec, ',
             sharedPtr^.lastLine: 1, ' lines, ',
             (sharedPtr^.lastLine * 60) div sharedPtr^.endsec: 1, ' lines/min');
    end;
    {>>>}
99:
  {<<<  close source files}
  while sharedPtr^.sourcelevel > 0 do
    begin
    close (sharedPtr^.source[sharedPtr^.sourcelevel]);
    sharedPtr^.sourcelevel := sharedPtr^.sourcelevel - 1;
    end;
  {>>>}
  {<<<  listing}
  if (sharedPtr^.lastlist > 0) then
    {<<<  not sure}
    begin
    if (sharedPtr^.listTable[sharedPtr^.lastlist].count = 0) then
      with sharedPtr^.listTable[sharedPtr^.lastlist] do
        count := sharedPtr^.lastLine - start + 1;
    end
    {>>>}
  else if sharedPtr^.lasterror > 0 then
    {<<<  still not sure}
    begin
    sharedPtr^.lastlist := sharedPtr^.lastlist + 1;
    with sharedPtr^.listTable[sharedPtr^.lastlist] do
      begin
      start := sharedPtr^.lastLine + 1;
      count := 0;
      end;
    end;
    {>>>}

  if not sharedPtr^.forceList or (sharedPtr^.lasterror > 0) then
    begin
    resetswitches;
    list;

    if sharedPtr^.lasterror > 0 then
      begin
      writeln ('Failed - errors:', sharedPtr^.lasterror: 1);
      exitst (exitstatus);
      end;
    end;
  {>>>}
end.
