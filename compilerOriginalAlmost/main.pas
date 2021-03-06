{ main.pas- main }
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
{$noindex} {there is a subscript error in warnat}
{>>>}
%include 'hdr.def';
%include 'hdrprc.def';

label 99;

type
  tempfileonetype = file of packed array [0..diskbufsize] of hostfilebyte;
  tempfiletwotype = file of packed array [0..diskbufsize] of hostfilebyte;

var
  { The following file declarations must be included in each pass, and must be the first variables declared in that pass }
  tempfileone: tempfileonetype; {interface file scan-analys, travrs-code}
  tempfiletwo: tempfiletwotype; {interface file analys-travrs}

{<<<}
function min;

  begin
    if i < j then min := i
    else min := j
  end;
{>>>}
{<<<}
function max;

  begin
    if i > j then max := i
    else max := j
  end;
{>>>}

{<<<}
procedure resetswitches;
{ reset switch table between passes }

  begin
    switchcounters := originalswitches;
    currentswitch := 1;
    putlow := 0;
    puthi := 0;
    getlow := 0;
    gethi := 0;
  end;
{>>>}
{<<<}
procedure warnat;
{ put an error message into errortable.
  Halt compilation if this is a fatal error, and force fatal error if errortable is filled by this error }

  begin
    if lasterror < errortablesize - 1 { Too many errors? } then
      lasterror := lasterror + 1 { No - next table entry }
    else { Yes - quit }
      begin
      fatalflag := true;
      column := 0;
      error := toomanyerrors;
      lasterror := errortablesize
      end;

    with errortable[lasterror] do
      begin
      err := error;
      errline := line;
      errcolumn := column
      end;

    if fatalflag then
      begin
      if scanalys then
        scan2
      else if stringtablelimit < stringfilecount + stringtabletop then
        scan2;
      goto 99;
      end;
  end;
{>>>}
{<<<}
procedure ovrlay;
{ call an overlay where caller resides at the same overlay level
  as the callee.  DEC operating systems do not allow this kind of call,
  thus caller uses this routine (which is at the root level) to do
  the work.  This approach works for all three major DEC operating systems. }

  begin
    case o of
      xopenl: callo(openl);
      xopennext: callo(opennext);
      xopens: callo(opens);
      xcloses: callo(closes);
      xclosel: callo(closel);
      xbody: callo(body);
      xcstruct: callo(cstruct);
      xgenblk: callo(genblk);
      xpanic: callo(panic);
      xopentree: callo(opentree);
      xclosetree: callo(closetree);
      xopenenv: callo(openenv);
      end;
  end {ovrlay} ;
{>>>}
{<<<}
procedure abort {msg: abortwarning} ;
{ abort in a panic, printing a message and killing the compile }

  begin
    abortmsg := msg;
    ovrlay (xpanic);
  end; {abort}
{>>>}

{$rangecheck}
{<<<}
procedure assertcode {(b: boolean)};

  var i: -10000..-1;

  begin
    if not b then
      begin
      if not switcheverplus[test] then i := codesection
      else writeln('assertion error');
      end;
  end;
{>>>}
{$norangecheck}

{<<<}
procedure setoptions;
{ set overall options based on genmask and command line }

var
  genitem: gentypes; { quasi induction for handling codegen mask }
  mask: unsignedint; { mask for handling codegen optimization options }
  maskbit: integer; { induction for checking optimization mask bits }
  disabled: boolean; { true if mask specifies disabling }

begin {setoptions}
  { set default code generation options set }
  genset := [firstgenopt..lastgenopt];

  if genoptmask < 0 then
    begin
    mask := abs(genoptmask);
    disabled := false;
    end
  else
    begin
    disabled := true;
    mask := genoptmask;
    end;

  { now set the options based upon -debug -profile }
  if switcheverplus[debugging] or switcheverplus[profiling] then
    begin
    genset := genset - [lifetimes, propagation, hoisting, removedeadcode,
              subexpressions, tailmerging, bitops];
    end;

  { if a codegen options mask was specified, update the set }
  if switcheverplus[genmask] then
    begin
    if switcheverplus[test] then
      begin
      if disabled then
        write('DISABLED: ')
      else
        write('ENABLED: ');
      end;

    genitem := firstgenopt;

    { walk through the mask }
    overrideset := [];

    for maskbit := 0 to 15 do
      begin
      if odd(mask) then
        begin
        overrideset := overrideset + [genitem];
        if switcheverplus[test] then
          write(maskbit: 3);
        end;

      mask := mask div 2;
      genitem := succ(genitem);
      end;

    if disabled then
      genset := genset - overrideset
    else
      genset := genset + overrideset;

    if switcheverplus[test] then
      writeln;
    end;
end {setoptions} ;
{>>>}
{<<<}
procedure settime;

begin {settime}
  if switcheverplus[timing] then
    timestamp (dum, dum, dum, istarthour, istartmin, istartsec);
end {settime} ;
{>>>}
{<<<}
procedure printtime (pass: packed array [l..h: shortint] of char);

var
  deltasec, deltamin, deltahour: integer; {to compute time per pass}
  i: shortint; {induction}

begin
  if switcheverplus[timing] then
    begin
    timestamp (dum, dum, dum, deltahour, deltamin, deltasec);
    if deltahour < istarthour then
      deltahour := deltahour + 24;
    deltasec := max (deltasec - istartsec + 60 * ((deltamin - istartmin) + 60 * (deltahour - istarthour)), 1);
    for i := l to h do
      write (pass[i]);
    writeln (' ', deltasec: 1, ' sec., ', lastline: 1, ' lines, ', (lastline * 60) div deltasec: 1, ' lines/min.');
    end;
end;
{>>>}

begin
  current_stmt := 0; { used by error }
  current_line := 0; { used by error }
  fatalflag := false;
  lasterror := 0;
  sourcelevel := 0;
  currentswitch := 1;
  genoptmask := 0;

  csi;
  setoptions;
  if switcheverplus[timing] then
    timestamp (dum, dum, dum, starthour, startmin, startsec);

  opentemp;

  originalswitches := switchcounters;
  if switcheverplus[listcount] then
    {<<<  reset listing counters}
    begin
    listtable[1].start := 1;
    listtable[1].count := 0;
    lastlist := 1;
    end
    {>>>}
  else
    lastlist := 0;

  if not scanalys then
    {<<<  scan}
    begin
    settime;
    scan;

    if switcheverplus[timing] and switcheverplus[details] then
      printtime ('scan  ');

    resetswitches;
    end;
    {>>>}

  opena;
  settime;

  if scanalys then
    scan1;
  analys;
  if scanalys then
    scan2;

  printtime ('analys');
  if (lasterror = 0) and
     (switcheverplus[outputmacro] or switcheverplus[outputobj]) then
    {<<<  generate code}
    begin
    resetswitches;

    opent;
    settime;
    if travcode then
      begin
      openc;
      initcode;
      end;

    travrs;
    if travcode then
      exitcode
    else
      begin
      if switcheverplus[timing] and switcheverplus[details] then
        printtime ('travrs');
      resetswitches;
      settime;
      openc;
      code;
      if switcheverplus[timing] and switcheverplus[details] then
        printtime ('code  ');
      end;

    closec;

    if switcheverplus[symboltable] then
      closed;

    if switcheverplus[timing] then
      begin
      timestamp (dum, dum, dum, endhour, endmin, endsec);
      if endhour < starthour then
        endhour := endhour + 24;
      endsec := max (endsec - startsec + 60 * ((endmin - startmin) + 60 * (endhour - starthour)), 1);
      writeln ('Total  ', endsec: 1, ' sec., ', lastline: 1, ' lines, ', (lastline * 60) div endsec: 1, ' lines/min.');
      end;
    end;
    {>>>}
  writeln (insertions:1, ' identifiers, ', proctabletop:1, ' procedures/functions declared');

99:
  closeall; { close all files except stringfile for listing }
  {<<<  listing}
  if (lastlist > 0) then
    begin
    if (listtable[lastlist].count = 0) then
      with listtable[lastlist] do
        count := lastline - start + 1;
    end
  else if lasterror > 0 then
    begin
    lastlist := lastlist + 1;
    with listtable[lastlist] do
      begin
      start := lastline + 1;
      count := 0;
      end;
    end;

  if not fakelist or (lasterror > 0) then
    begin
    resetswitches;
    list;
    closel;
    if lasterror > 0 then
      begin
      writeln ('?Errors detected: ', lasterror: 1);
      exitst (exitstatus); {value is opsys dependent}
      end;
    end;
  {>>>}
  close_str; { close stringfile }
end.
