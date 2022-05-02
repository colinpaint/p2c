{ scan.pas }
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

}

{**************************************************************}
{                                                              }
{                Lexical Scanner for Pascal-2                  }
{                                                              }
{**************************************************************}

{ Pascal 2 Lexical scanner --

  This routine reads the input file, processing "include" directives,
  and breaks it up into tokens.  As each token is scanned, it is written
  to an intermediate file for use by analys.

  Identifiers are looked up using a hash method, with the actual text
  of the identifier kept in a large table in the heap.  This table is
  written to the "string file" when the pass is done.

  Strings are written to the "string file" as they are scanned, and
  are handled by reference to their addresses within this file.
}
{>>>}
{$nomain}
{<<<  includes}
%include 'common.def';
%include 'token.def';
%include 'main.def';

%include 'scan.def';
{>>>}
{<<<}
const
  { Machine Dependent parameters for scanner }
  maxscanswitch = 40; { max number of recognized embedded switches }
  maxscanswitchlen = 14; { max length is NoPointerCheck }

  { Language specific parameters for scanner }
  tabspace = 8; { The number of spaces a tab input becomes }

  reservedcount = 42; {Number of reserved words}
  minreslen = 2; {Length of shortest reserved word}
  maxreslen = 9; {Length of longest reserved word}
  maxreslen1 = 10; {maxreslen + 1}

  { special characters - ignore nul and convert tabch, formfeed to ' ' }
  nul = 0;
  tabch = 9;
  formfeed = 12;
  cr = 13;
  lowercase = 32; {difference between ord of upper case and lower case char}

  { real number conversion parameters }
  DECformat = 0; { format used by PDP-11 and VAX }
  IEEEformat = 256; { format used by M68000 and N16000 }
  INTELformat = 512; { same as IEEE, except result gets stored backwards }
  IBMformat = 768; { hexadecimal format used by 360/370/31xx/43xx, etc. }

  SinglePrecision = 0;
  DoublePrecision = 16;
  QuadPrecision = 32; { not currently implemented }

  inputbufsize = 40; {length of input line if using fixed arrays}

  { read real }
  bitsperbyte   =     8 ;       { number of bits per binary byte }
  nibblesize    =    16 ;       { number of elements in nibble }
  bytesize      =   256 ;       { number of elements in byte }
  { wordsize      = 65536 }       { number of elements in word }
  { maxnibble     =    15 }       { nibblesize - 1 }
  maxbyte       =   255 ;       { bytesize - 1  }
  maxuword       = 65535 ;       { wordsize - 1 }

  halfwordsize  = 32768 ;       { wordsize div 2 }
  halfmaxword   = 32767 ;       { maxword div 2 }

  maxrealbytes  =     8 ;       { maximum number of bytes per real }
  maxrealbits   =    64 ;       { maximum number of bits per real }
{>>>}
{<<<}
type
  lineindex = 0..linelen; {index into input line}

  halfrealsize = 0..65535; {subrange which is half size of single real}

  alfa = packed array [1..10] of char; {to hold directives}

  reswordtype = packed array [1..maxreslen] of char; {reserved word spelling}
  reservedindex = 1..reservedcount; {index for reserved words}
  identifierrange = 0..1000; {some arbitrary range}

  { scan switches }
  internalswitch = (codesectsw, modulesw, identsw, xshortsectsw, xversionsw, xsectionsw);
  scanswitchindex = 0..maxscanswitch; {switchtable switch}
  switchvalue = - maxswitchvalue..maxswitchvalue; {bumpvalue}
  switchname = packed array [1..maxscanswitchlen] of char; {name of switch}
  {<<<}
  scanswitchentry = record
                      n: switchname; {name of switch}
                      case internal: boolean of
                        true: (is: internalswitch; {internal switch label} );
                        false:
                          (s: switch; {switch for which this is the entry}
                           v: switchvalue;
                           {amount to increment counter} );
                    end;
  {>>>}
  {<<<}
  { file begins with scanner globals (en_scan_var_block), followed by
    compressed hash table (ehashblock, where ehashblock.pos < 0 means
    that - ehashblock.pos zero entries were compressed from the file),
    and finally the stringtable.
  }
  switchblock = packed record
                  s: switch;
                  v: switchvalue;
                end;
  {>>>}

  {<<<}
  hashtablerecord = record
                      pos: stringindex; {string index of id text}
                      len: columnindex; {length of id text}
                      key: hashindex {key assigned to this id}
                    end;
  {>>>}
  {<<<}
  hashtableblock = packed record
                     pos: -hashtablesize .. stringtablesize;
                     len: columnindex; {length of id text}
                     key: hashindex {key assigned to this id}
                   end;
  {>>>}

  { readreal }
  byte          = 0..maxbyte ;
  word          = 0..maxuword ;
  RealFormat    = (DECformat1 , IEEEformat1 , INTELformat1 , IBMformat1);
  RealPrecision = (SinglePrecision1 , DoublePrecision1 , QuadPrecision1);
  RealRounding  = (ToNearest, ToZero, ToPosInf, ToNegInf);
  RealClass     = (ZeroClass, NormalClass, DenormalClass, InfClass, QNaNClass, SNaNClass);
{>>>}
{<<<}
var
  { shared vars }
  sharedPtr: sharedPtrType;
  tokenSharedPtr : tokenSharedPtrType;

  nextch: char;        { next character to be scanned }
  ch: char;            { current character being scanned }
  unconvertedch: char; { current character in raw (not case converted) state }

  linepos: lineindex;  { current position in input line }
  currentbuf: packed array [1..inputbufsize] of char; { input buffer }

  charcount: columnindex; { effective position of nextch in this line }
  chpos: columnindex;     { effective position of "ch" in this line }
  oldchpos: columnindex;  { effective position of last "ch" }

  new_filepos: integer;     { for tracking fileposition }
  current_filepos: integer; { new_filepos, delayed by one character }

  lasttokenline: integer; { line of last token read }
  lastbaseline: integer;  { baseline for last file read }
  endofline: boolean;     { effective eoln with nextch }
  endofinput: boolean;    { effective overall eof }

  convertingcase: boolean; { true if uppercase wanted (except in strings) }
  skippingblanks: boolean; { currently skipping blanks }

  saveinput: array [1..sourcedepth] of
      record
        savech: char; { nextch for pushed source levels }
        saveendofline: boolean;
        savebuf: packed array [1..inputbufsize] of char;
        savepos: lineindex;
        savefileindex: integer;
        savefilename_length: filenameindex;
      end;
  baseline: array [1..sourcedepth] of integer; {starting line for current file}

  tokenbufindex: 0..diskbufsize; { next available space in token file block }

  { stringbuf[curstringbuf] is used to buffer the current quoted string (nexttoken),
    while stringbuf[not curstringbuf] is the buffer for the previous (token) string, if any. }
  stringbuf: array [boolean] of packed array [0..linelen] of char;

  { hashtable, includes pointer to loc in string table }
  hashtable: array [hashindex] of hashtablerecord;
  scanswitchtable: array [scanswitchindex] of scanswitchentry; { table of embedded switch names }

  { table of reserved words, by length, and pointers into that table pointers to reswords }
  reslentable: array [minreslen..maxreslen1] of reservedindex;
  reswords: array [reservedindex] of reswordtype; {text of reswords}
  reswordtokens: array [reservedindex] of tokentype; {tokens for reswords}
  tokentable: array [')'..'^'] of tokentype; {tokens for single char tokens}

  mapchars: packed array ['A'..'Z'] of char; {lower to upper conversion table}
  incomment: boolean; {used to detect an unfinished comment}
  inliteralstring: boolean; {used to detect an unfinished literal string}
  first_real_seen: boolean; {used to detect error when $double occurs after first real constant.}
  first_token_seen: boolean; {used to detect error when $case occurs after the first token.}
  curFileIndex: integer; { pointer to current filename }

  { switch buffers used to delay effect of switches processed while reading nexttoken until becomes thistoken }
  nextswitchread: boolean; {set true when first switch within comment is found}
  nextswitchcounters: switchcounterarray; {buffer for switchcounters}
  nextswitcheverplus: switcheverplusarray; {buffer for switcheverplus}
{>>>}

{<<<}
procedure puttoken;
{<<<}
{ Put the current token to the token file.
  This is encoded to the hostfilebyte level, with only the data required being sent.
  This reduces the amount of intermediate file I/O required,
  resulting in a significant speedup on machines with slow disks.

  Since this is a high-bandwidth spot, code put sequences are written
  in line rather than being isolated in procedures.

  Assumptions built in are:
        1.  "left" and "right" will fit in a hostfilebyte
        2.  a target character will fit in a hostfilebyte

  These seem likely enough to be true that the speed-up available by making these assumptions is worth taking
}
{>>>}

var
  dif: hostfilebyte; {difference in line numbers}

  {<<<}
  procedure puttempfile;
  { Does the equivalent of a put on the token file.  Actually the file is
    a file of blocks, and an actual put is done only if the block is full }

  begin
    if tokenbufindex = diskbufsize then
      begin
      tokenbufindex := 0;
      put (tokenSharedPtr^.tokenFile);
      end
    else
      tokenbufindex := tokenbufindex + 1;
  end;
  {>>>}
  {<<<}
  procedure putint (i: integer {value to put} );
  { Puts an integer value to the token file as successive bytes. }

  var
    { This fudges an integer into bytes.  The constant "32" is }
    { simply a large enough number to include all probable systems. }
    fudge:
      record
        case boolean of
          true: (int: integer);
          false: (byte: packed array [1..32] of hostfilebyte);
      end;

    j: 1..32;

  begin
    if (i >= 0) and (i < hostfilelim) then
      begin
      tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte := i;
      if tokenbufindex = diskbufsize then
        begin
        tokenbufindex := 0;
        put (tokenSharedPtr^.tokenFile);
        end
      else
        tokenbufindex := tokenbufindex + 1;
      end
    else
      begin
      tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte := hostfilelim;
      puttempfile;
      fudge.int := i;
      for j := 1 to hostintsize * hostfileunits do
        begin
        tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte := fudge.byte[j];
        puttempfile;
        end;
      end;
  end;
  {>>>}
  {<<<}
  procedure putreal;
  { Put a real value to the token file as successive bytes }

  var
    { this fudges a real into a bytes.  The constant "32" is }
    { simply a large enough number to include all probable systems. }
    fudge:
      record
        case boolean of
          true: (rl: realarray);
          false: (byte: packed array [1..32] of hostfilebyte);
      end;
    j: 1..32; {induction var}


  begin {putreal}
    for j := 1 to 32 do fudge.byte[j] := 0;
    fudge.rl := tokenSharedPtr^.nexttoken.realvalue;
    for j := 1 to size(realarray) do
      begin
      tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte := fudge.byte[j];
      puttempfile;
      end;
  end;
  {>>>}

begin
  tokenSharedPtr^.tokenCount := tokenSharedPtr^.tokenCount + 1;

  with tokenSharedPtr^.nexttoken do
    begin
    { Put line increments as necessary }
    while lasttokenline < line do
      begin
      dif := min(line - lasttokenline, hostfilelim);
      if dif = 1 then
        begin
        tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke := lineinc;
        if tokenbufindex = diskbufsize then
          begin
          tokenbufindex := 0;
          put (tokenSharedPtr^.tokenFile);
          end
        else
          tokenbufindex := tokenbufindex + 1;
        end
      else
        begin
        tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke := lineadd;
        puttempfile;
        tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke := dif;
        puttempfile;
        end;
      lasttokenline := lasttokenline + dif;
      end;

    if lastbaseline <> baseline then
      begin
      tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke := newfile;
      puttempfile;
      putint (baseline);
      putint (fileindex); { filename pointer }
      lastbaseline := baseline;
      end;

    tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke := token;
    if tokenbufindex = diskbufsize then
      begin
      tokenbufindex := 0;
      put (tokenSharedPtr^.tokenFile);
      end
    else
      tokenbufindex := tokenbufindex + 1;

    tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke := left;
    if tokenbufindex = diskbufsize then
      begin
      tokenbufindex := 0;
      put (tokenSharedPtr^.tokenFile);
      end
    else
      tokenbufindex := tokenbufindex + 1;

    if token in [ident, intconst, realconst, dblrealconst, charconst,
       stringconst] then
      begin
      tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke := right;
      if tokenbufindex = diskbufsize then
        begin
        tokenbufindex := 0;
        put (tokenSharedPtr^.tokenFile);
        end
      else
        tokenbufindex := tokenbufindex + 1;

      case token of
        {<<<}
        ident:
          begin
          putint(key);
          putint(keypos);
          end;
        {>>>}
        {<<<}
        intconst:
          putint(intvalue);
        {>>>}
        {<<<}
        charconst:
          begin
          tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke := intvalue;
          puttempfile;
          end;
        {>>>}
        {<<<}
        realconst, dblrealconst:
          putreal;
        {>>>}
        {<<<}
        stringconst:
          begin
          putint(pos);
          putint(len);
          end;
        {>>>}
        end
      end;
    end
end;
{>>>}
{<<<}
procedure puttokenfile;
{ Do the equivalent of a put to the token file.  The value of the global
  variable "token" is encoded by "puttoken" and written in a packed format
  to the tokenfile.  A count of tokens is kept and used to indicate the
  place in the file where embedded switches are found.
}
begin
  { update token count -- two words for small computers }
  if sharedPtr^.putlow = maxint then
    begin
    sharedPtr^.putlow := 0;
    sharedPtr^.puthi := sharedPtr^.puthi + 1
    end
  else
    sharedPtr^.putlow := sharedPtr^.putlow + 1;

  puttoken;
end;
{>>>}

{<<<}
procedure seekstringfile (n: integer);
{ Do the equivalent of a "seek" on the string file.
  This sets the file and "nextstringfile" to access byte "n" of the stringfile.
  only meaningful when scan and analys are operating as one pass
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
procedure putstringfile;
{ Do the equivalent of a "put" on the stringfile }

begin
  if sharedPtr^.nextstringfile = diskbufsize then { string buffer full, write it out }
    begin
    sharedPtr^.nextstringfile := 0;
    sharedPtr^.curstringblock := sharedPtr^.curstringblock + 1;
    new (sharedPtr^.stringblkptr);
    sharedPtr^.stringblkptrtbl[sharedPtr^.curstringblock] := sharedPtr^.stringblkptr;
    end
  else
    sharedPtr^.nextstringfile := sharedPtr^.nextstringfile + 1; { not full, update pointer }
end;
{>>>}
{<<<}
procedure dumpidentifiers;
{ Dumpidentifiers -- dumps stringtable into stringfile.
  Analys and Code occasionally need the character representation of an identifier. }

var
  i: stringindex;

begin
  for i := 1 to sharedPtr^.stringtabletop do
    begin
    sharedPtr^.stringblkptr^[sharedPtr^.nextstringfile] := ord(sharedPtr^.stringtable^[i]);
    putstringfile;
    end;
end;
{>>>}

{<<<}
procedure scanFatal (err: warning);
{ Issue an error message and set the "fatalflag" to stop compilation }

begin
  sharedPtr^.fatalflag := true;

  seekstringfile (sharedPtr^.stringfilecount);
  dumpidentifiers;
  close (sharedPtr^.source[sharedPtr^.sourcelevel]);

  sharedPtr^.sourcelevel := 0;
  warnat (err, sharedPtr^.lastline, chpos);
end;
{>>>}
{<<<}
procedure logInputFilename;
{ Put the name of the file just opened found in global "filename", with its length in global "filename_length"
  in the string table, among the identifiers. The name in the string table is terminated by a chr(0).

  On systems that use logical names or file versions,
  and where we have provided a routine to translate filenames, we use that routine,
  and put the fully qualified name in the string table.

  This assures that LIST will be accessing the same file that SCAN accessed,
  even if the user has edited the file during compilation, and created a new version.

  This also handles the case of a file being created during compilation
  with the same name as one that was included via the /include= switch,
  when the new file is in a "closer" directory
}
var
  p: filerememberptr; {induction on filerememberlist}
  q: filerememberptr; {points to item to be added}
  i: FilenameIndex; {induction on file name}
  temp: shortint; { temporary file name length }

begin
  curFileIndex := sharedPtr^.stringtabletop + 1;

  {*** expand filename *** }
  new (q);
  q^.next := nil;
  q^.offset := curFileIndex;
  if sharedPtr^.sourcelevel = 1 then
    tokenSharedPtr^.nexttoken.baseline := sharedPtr^.lastline - 1;

  if sharedPtr^.filerememberlist <> nil then
    begin
    p := sharedPtr^.filerememberlist;
    while p^.next <> nil do
      p := p^.next;
    p^.next := q;
    end
  else
    sharedPtr^.filerememberlist := q;

  if sharedPtr^.stringtabletop + sharedPtr^.filename_length >= stringtablesize then
    scanFatal (stringtableoverflow);

  for i := 1 to sharedPtr^.filename_length do
    begin
    sharedPtr^.stringtabletop := sharedPtr^.stringtabletop + 1;
    sharedPtr^.stringtable^[sharedPtr^.stringtabletop] := sharedPtr^.filename[i];
    end;

  sharedPtr^.stringtabletop := sharedPtr^.stringtabletop + 1;
  sharedPtr^.stringtable^[sharedPtr^.stringtabletop] := chr(0);
end;
{>>>}

{<<<}
procedure getch;
{ Transfers the global "nextch" to "ch" and gets a new value for "nextch".
  This always provides a one character lookahead to simplify some of the lexical scanning.
  In the process, the globals "endofinput", "endofline", and "chpos" are updated.
  Formfeed and tab characters are converted to blanks.
  The source files are organized as a stack of files, and input is always from the top of the stack.
  If end of included file is found, stack popped to including source file
}
  {<<<}
  procedure special;

  begin
    if (nextch < ' ') and (nextch <> chr(tabch)) then
      begin
      if nextch = chr(formfeed) then
        begin
        { formfeed }
        endofline := true; { but ends line as well }
        nextch := ' '; { also becomes space }
        end
      else if (nextch <> chr(nul)) and (nextch <> chr(cr)) then
        begin
        { other control character, warn and ignore }
        warnat (badchar, sharedPtr^.lastline, charcount - 1);
        nextch := ' ';
        end
      end
    else
      charcount := charcount + 1;
  end;
  {>>>}
  {<<<}
  procedure dolinetoolong;
  {Line too long, issue warning}

  begin
    warnat (linetoolong, sharedPtr^.lastline, linelen);
    sharedPtr^.lastline := sharedPtr^.lastline + 1;
    charcount := 1;
  end;
  {>>>}
  {<<<}
  procedure getnextch;

  begin
    if linepos >= inputbufsize then
      begin
      read (sharedPtr^.source[sharedPtr^.sourcelevel], currentbuf);
      linepos := 0;
      end;

    linepos := linepos + 1;
    nextch := currentbuf[linepos];

    if (nextch < ' ') then
      special
    else
      charcount := charcount + 1;

    if charcount > linelen + inputbufsize then
      dolinetoolong;
  end;
  {>>>}
  {<<<}
  procedure dotab;

  begin
    {becomes space, update column}
    ch := ' ';

    if not inliteralstring or incomment then
      begin
      charcount := min(((charcount - 1) div tabspace) * tabspace + tabspace, linelen);
      chpos := charcount;
      end
    else
      warnat (badchar, sharedPtr^.lastline, chpos);
  end;
  {>>>}
  {<<<}
  procedure doeof;

  begin
    if sharedPtr^.sourcelevel > 1 then
      begin {inside include, pop to parent level}
      close (sharedPtr^.source[sharedPtr^.sourcelevel]);
      with saveinput[sharedPtr^.sourcelevel] do
        begin
        nextch := savech; {restore next char to read}
        currentbuf := savebuf; {restore input line}
        linepos := savepos; {restore index in line}
        curFileIndex := savefileindex;
        endofline := saveendofline;
        sharedPtr^.filename_length := savefilename_length;
        end;

      tokenSharedPtr^.nexttoken.baseline := sharedPtr^.lastline - baseline[sharedPtr^.sourcelevel];
      if endofline then
        sharedPtr^.lastline := sharedPtr^.lastline + 1;
      sharedPtr^.sourcelevel := sharedPtr^.sourcelevel - 1; {pops the source stack}
      end

    else
      begin {end of main source file, so quit}
      sharedPtr^.curfile := sharedPtr^.curfile + 1;
      openNext;
      if sharedPtr^.morefiles then
        begin
        logInputFilename;
        linepos := inputbufsize;
        getnextch;
        end
      else
        begin
        endofinput := true;
        nextch := ' ';
        skippingblanks := false;
        end;
      end

  end;
  {>>>}
  {<<<}
  procedure doeoln;

  var
    i1, i2 : integer;

  begin
    { Will nextch come from new line ? }
    if nextch = ' ' then
      begin
      readln (sharedPtr^.source[sharedPtr^.sourcelevel]); {skip past eoln}
      linepos := inputbufsize;
      if (sharedPtr^.sourcelevel = 1) or not eof (sharedPtr^.source[sharedPtr^.sourcelevel]) then
        begin
        endofline := true;
        sharedPtr^.lastline := sharedPtr^.lastline + 1;
        end;
      end
    else
      begin {hack for eof ?????????????}
      nextch := ' ';
      charcount := charcount + 1;
      skippingblanks := false;
      end

  end;
  {>>>}
  {<<<}
  procedure doendofline;

  begin
    charcount := 0; {this char is beginning of new line}
    endofline := false;
  end;
  {>>>}

begin
  repeat
    { Move to next character }
    ch := nextch;
    oldchpos := chpos;
    chpos := charcount;
    current_filepos := new_filepos;

    if ch = chr(tabch) then
      dotab;

    unconvertedch := ch;  { saved for case switch }
    if convertingcase and (ch in ['A'..'Z']) then
      ch := mapchars[ch];
    if endofline then
      doendofline;

    if linepos < inputbufsize then
      begin
      { normal case -- fill nextch }
      linepos := linepos + 1;
      nextch := currentbuf[linepos];
      if (nextch < ' ') then
        special
      else
        charcount := charcount + 1;
      if charcount > linelen + inputbufsize then
        dolinetoolong;
      end
    else if eof (sharedPtr^.source[sharedPtr^.sourcelevel]) then
      doeof
    else if eoln (sharedPtr^.source[sharedPtr^.sourcelevel]) then
      doeoln
    else
      getnextch;
  until (not skippingblanks) or (ch <> ' ');
end;
{>>>}

{ interface }
{<<<}
procedure dumpstr (len: lineindex; buf, dumplen: boolean);
{ Copy stringbuf[buf] to the string file }

var
  i: 1..linelen;

begin
  seekstringfile (sharedPtr^.stringfilecount);
  sharedPtr^.stringfilecount := sharedPtr^.stringfilecount + len;

  if dumplen then
    begin
    sharedPtr^.stringblkptr^[sharedPtr^.nextstringfile] := ord (stringbuf[buf, 0]);
    putstringfile;
    len := len - 1;
    end;

  for i := 1 to len do
    begin
    sharedPtr^.stringblkptr^[sharedPtr^.nextstringfile] := ord (stringbuf[buf, i]);
    putstringfile;
    end;
end;
{>>>}
{<<<}
procedure scantoken;
{ Get the next token from the source file. main procedure in scan, and converts all tokens to internal format. }

var
  somethingelse: boolean; { set true when we have skipped all separators }
  commentch: char; { Comment terminator (needed for obsolete comments)}

  {<<<}
  procedure skipcomment;
  { Skip characters until the appropriate ending comment bracket is found.
    If the comment contains compiler switches (first character "$), these
    are parsed and the new value set into the switch table
  }
  var
    termchars: set of char; {terminating characters}

    {<<<}
    procedure scanswitches;
    { Scans a list of compiler switches, separated by commas, and checks them against the known switches.
      Switches must match exactly on all characters provided,
      with a minimum of three characters needed to distinguish a switch.
      Unknown switches are ignored, as distasteful as this might seem.
      If a switch is found, the existence of the switch is logged in the
      "switchtable" along with the token count so that other passes can use
      it.  In addition, the switch counters are updated for this pass, and
      "switcheverplus" is set if the switch is turned on.
      We are actually reading the next token while
      processing the current token so we delay the effect of counting switches
      (this normally happens while analys is reading in the intermediate file).
      If this switch affects the state of the listing, an entry is made in the global "listtable" for the listing pass. }

    const
      checkswitchcount = 5; {nr of checking switches}

    var
      i, j, l: identifierrange; {induction var for search}
      oldcount: identifierrange; {old value of switchcounter}
      slen: identifierrange; {length of switch as read}
      sname: switchname; {holds switch name}
      nofound: boolean; {negated switch}

      {<<<}
      function match: boolean;
      { Match the switch string in sname against the switch name in the global constant "scanswitchtable[i]" }

      var
        j: identifierrange; {induction variable for match}

      begin
        with scanswitchtable[i] do
          begin
          j := 1;
          while (j < slen) and (sname[j] = n[j]) do j := j + 1;
          match := (j = slen);
          end;
      end;
      {>>>}
      {<<<}
      procedure stuff;
      { Insert the current character into the stringtable if there is room }

      begin
        if sharedPtr^.stringtabletop < stringtablesize then
          begin
          sharedPtr^.stringtabletop := sharedPtr^.stringtabletop + 1;
          sharedPtr^.stringtable^[sharedPtr^.stringtabletop] := nextch
          end
        else
          scanFatal (stringtableoverflow);
      end;
      {>>>}
      {<<<}
      function snumber: integer;
      { Read a number for a scanner switch. }

      var
        val: integer;

      begin
        val := 0;
        while (nextch >= '0') and (nextch <= '9') do
          begin
          val := val * 10 + (ord(nextch) - ord('0'));
          getch;
          end;
        snumber := val;
      end;
      {>>>}
      {<<<}
      procedure addswitch (i: scanswitchindex {switchentry to add});
      {add entry i to the switch table, the table of consecutive switches encountered }

      begin
        if sharedPtr^.lastswitch < switchtablesize {table full ?} then {no}
          begin {all other switches}
          sharedPtr^.lastswitch := sharedPtr^.lastswitch + 1;
          with sharedPtr^.switches[sharedPtr^.lastswitch] do
            begin
            s := scanswitchtable[i].s;
            v := scanswitchtable[i].v * ((1 - ord(nofound)) * 2  - 1);
            mlow := sharedPtr^.putlow;
            mhi := 0;
            if false then
              mhi := sharedPtr^.puthi; {mark for future passes}
            end;
          end; {note that we ignore switch if table full}
      end;
      {>>>}
      {<<<}
      procedure bumpswitch (s: switch; {switch to bump} v: switchvalue {value to use});
      {bump switch count for switch s }

      begin
        if (s <> listcount) then
          begin {track look-ahead}
          if not nextswitchread then
            begin
            nextswitchread := true;
            nextswitchcounters := sharedPtr^.switchcounters;
            nextswitcheverplus := sharedPtr^.switcheverplus;
            end;

          nextswitchcounters[s] := nextswitchcounters[s] + v;
          if nextswitchcounters[s] > 0 then
            nextswitcheverplus[s] := true;
          end

        else
          begin
          oldcount := sharedPtr^.switchcounters[s]; {old value}
          sharedPtr^.switchcounters[s] := sharedPtr^.switchcounters[s] + v;
          if sharedPtr^.switchcounters[s] > 0 then
            sharedPtr^.switcheverplus[s] := true;

          {set nextswitch buffer in case we've just read a switch}
          nextswitchcounters[s] := sharedPtr^.switchcounters[s];
          nextswitcheverplus[s] := sharedPtr^.switcheverplus[s];
          end;

      end;
      {>>>}

    begin
      repeat
        getch; {nextch in ['$',',']}

        if ch = ',' then {skip blanks following ','}
          while nextch = ' ' do getch;

        { Build switch name from source file }

        slen := 0;
        while nextch in ['a'..'z', 'A'..'Z', '0'..'9'] do
          begin
          slen := slen + 1;
          getch;
          if slen <= maxscanswitchlen then
            sname[slen] := ch;
          end;

        { Search for legal switches -- unknown are ignored }

        scanswitchtable[0].n := sname;
        scanswitchtable[0].internal := false;
        scanswitchtable[0].s := noswitch;
        scanswitchtable[0].v := 0;
        i := maxscanswitch;
        nofound := false;
        if (slen < 3) or (slen > maxscanswitchlen) then
          i := 0
        else
          begin
          if (sname[1] = 'n') and (sname[2] = 'o') then
            begin
            nofound := true;
            for l := 3 to slen do sname[l - 2] := sname[l];
            scanswitchtable[0].n := sname;
            slen := slen - 2;
            end;
          while not match do
            i := i - 1; {sentinel search}
          end;

        with scanswitchtable[i] do
          begin
          if s = fpc68881 then
            begin
            sharedPtr^.switcheverplus[cpu68020] := true;
            nextswitcheverplus[cpu68020] := true;
            end;

          if nofound and (internal or (s in [cpu8086, cpu68000, cpu68020, own, multidef])) then
            i := 0 {ignore illegal no's}
          else if not internal then
            begin
            { Detect the case where $double occurs after the first token }
            if (s = doublereals) and first_token_seen then
              if sharedPtr^.switchcounters[standard] <= 0 then
                warnat (baddouble, sharedPtr^.lastline, chpos)
              else
                i := 0; {ignore if standard active}

            { Detect the case where $case occurs after the first token }
            if (s = caseswitch) and first_token_seen then
              if sharedPtr^.switchcounters[standard] <= 0 then
                warnat(badcase, sharedPtr^.lastline, chpos)
              else
                i := 0; {ignore if standard active}
            end;
          end;

        { Process switch }
        { i = 0 if switch not found, but zero was set up above }
        with scanswitchtable[i] do
          if internal then
            {<<<  internal switch}
            begin
            if nextch = '=' then
              begin
              getch;
              case is of
                {<<<}
                codesectsw:
                  begin
                  sharedPtr^.codesect_string := sharedPtr^.stringtabletop + 1;
                  if nextch = '''' then
                    begin
                    convertingcase := false;
                    inliteralstring := true;

                    getch;
                    repeat
                      while (nextch <> '''') and not endofline do
                        begin
                        stuff;
                        getch;
                        end;
                      if not endofline then
                        begin
                        getch;
                        stuff;
                        end;
                    until (nextch <> '''') or endofline;

                    sharedPtr^.codesect_strlength := sharedPtr^.stringtabletop - sharedPtr^.codesect_string;

                    inliteralstring := false;
                    convertingcase := true;
                    end;
                  end;
                {>>>}
                {<<<}
                modulesw:
                  begin
                  sharedPtr^.module_string := sharedPtr^.stringtabletop + 1;
                  if nextch = '''' then
                    begin
                    convertingcase := false;
                    inliteralstring := true;

                    getch;
                    repeat
                      while (nextch <> '''') and not endofline do
                        begin
                        stuff;
                        getch;
                        end;
                      if not endofline then
                        begin
                        getch;
                        stuff;
                        end;
                    until (nextch <> '''') or endofline;

                    sharedPtr^.module_strlength := sharedPtr^.stringtabletop - sharedPtr^.module_string;

                    inliteralstring := false;
                    convertingcase := true;
                    end;
                  end;
                {>>>}
                {<<<}
                identsw:
                  begin
                  sharedPtr^.ident_string := sharedPtr^.stringtabletop + 1;
                  if nextch = '''' then
                    begin
                    convertingcase := false;
                    inliteralstring := true;

                    getch;
                    repeat
                      while (nextch <> '''') and not endofline do
                        begin
                        stuff;
                        getch;
                        end;
                      if not endofline then
                        begin
                        getch;
                        stuff;
                        end;
                    until (nextch <> '''') or endofline;

                    sharedPtr^.ident_strlength := sharedPtr^.stringtabletop - sharedPtr^.ident_string;

                    inliteralstring := false;
                    convertingcase := true;
                    end;
                  end;
                {>>>}
                xsectionsw:
                  sharedPtr^.codesection := snumber mod 16;
                {<<<}
                xshortsectsw:
                  begin
                  sharedPtr^.shortsection := true;
                  sharedPtr^.codesection := snumber mod 16;
                  end;
                {>>>}
                {<<<}
                xversionsw:
                  begin
                  sharedPtr^.objversion := snumber mod 256;
                  if nextch = '.' then
                    begin
                    getch;
                    sharedPtr^.objrevision := snumber mod 256;
                    end;
                  end;
                {>>>}
                end {case} ;
              end;
            end
            {>>>}
          else
            begin
            if i = checkswitchcount + 1 then {check switch}
              for j := 1 to checkswitchcount do
                bumpswitch (scanswitchtable[j].s, ((1-ord(nofound)) * 2  - 1))
            else
              bumpswitch (s, v * ((1 - ord(nofound)) * 2  - 1));
            if s = listcount then
              {<<<  list and nolist are entered into a special table, 'listtable'}
              begin
              if (oldcount + sharedPtr^.switchcounters[listcount] = 1) and
                 sharedPtr^.switcheverplus[listcount] then
                { Transition occurred }
                if (oldcount = 0) and (sharedPtr^.lastlist < listtablesize) then
                  begin
                  sharedPtr^.lastlist := sharedPtr^.lastlist + 1;
                  sharedPtr^.listtable[sharedPtr^.lastlist].start := sharedPtr^.lastline;
                  sharedPtr^.listtable[sharedPtr^.lastlist].count := 0;
                  end

                else if oldcount > 0 then
                  sharedPtr^.listtable[sharedPtr^.lastlist].count :=
                    sharedPtr^.lastline - sharedPtr^.listtable[sharedPtr^.lastlist].start + 1;
              end
              {>>>}
            else
              begin
              if (s = own) and (nextch = '=') then
                begin
                getch;

                if (targetopsys = vdos) and (nextch >= '0') and (nextch <= '9') then
                  sharedPtr^.datasection := snumber mod 16
                else
                  begin
                  sharedPtr^.ownsect_string := sharedPtr^.stringtabletop + 1;
                  if nextch = '''' then
                    begin
                    convertingcase := false;
                    inliteralstring := true;
                    getch;
                    repeat
                      while (nextch <> '''') and not endofline do
                        begin
                        stuff;
                        getch;
                        end;
                      if not endofline then
                        begin
                        getch;
                        stuff;
                        end;
                    until (nextch <> '''') or endofline;
                    sharedPtr^.ownsect_strlength := sharedPtr^.stringtabletop - sharedPtr^.ownsect_string;
                    inliteralstring := false;
                    convertingcase := true;
                    end;
                  end;
                end;
              if i = checkswitchcount + 1 then
                begin {check switch}
                for j := 1 to checkswitchcount do
                  addswitch(j);
                end
              else
                addswitch(i);
              end;
            end;
      until nextch <> ','; {process entire list}
    end;
    {>>>}

  begin
    incomment := true;

    { Check for switch stream and handle if needed }
    termchars := ['}', ')', commentch];
    if nextch = '$' then
      scanswitches;

    convertingcase := false;
    repeat
      getch;
      skippingblanks := true;
      while not ((ch in ['}', '*']) or endofinput) do
        getch; {bump to '*'}
      skippingblanks := false;
      while (ch = '*') and not endofinput do
        getch; {ignore multiples}
      if ch in termchars then
        incomment := false;
    until not incomment or endofinput;
    convertingcase := true;

    if incomment then
      warnat (eofincomment, sharedPtr^.lastline - 1, chpos)
    else if not endofinput then
      getch; {skip past delimiter if present}

    incomment := false;
  end;
  {>>>}
  {<<<}
  procedure stringliteral (quotech: char);
  { Scan a quoted string, building a string or character literal.
    As the string is read, a copy of it is built up in the string table.
    If the string length is exactly one, the token is returned as a character constant,
    otherwise the string is written to the string file and the token is a string constant
  }
  var
    stringpos: lineindex; { start of the string in the stringtable}
    stringlen: lineindex; { string length}
    i: lineindex;         { induction var}
    tempch: char;         { temporary holder for ch}

    {<<<}
    procedure stuff;
    { Insert the current character into the stringtable if there is room. }

    begin
      if stringpos < linelen then
        begin
        stringpos := stringpos + 1;
        stringbuf[sharedPtr^.curstringbuf, stringpos] := ch
        end
      else
        warnat (longstring, sharedPtr^.lastline, chpos);
    end;
    {>>>}

  begin
    { First read in the string and copy to stringtable }
    sharedPtr^.curstringbuf := not sharedPtr^.curstringbuf;
    stringpos := 0;
    repeat
      convertingcase := false; {we want a literal copy}
      inliteralstring := true;
      getch;
      while (ch <> quotech) and not endofline do
        begin
        stuff;
        getch;
        end;
      inliteralstring := false;
      convertingcase := true;
      if endofline then
        warnat (longstring, sharedPtr^.lastline - 1, chpos)
      else
        begin
        getch;
        stuff;
        end;
    until (ch <> quotech) or endofline;

    stringpos := stringpos - 1;
    stringbuf[sharedPtr^.curstringbuf, 0] := chr(stringpos);

    with tokenSharedPtr^.nexttoken do {check length and set returned token}
      if (stringpos = 0) and sharedPtr^.switcheverplus[standard] then
        warnat (zerostring, line, left)
      else if stringpos = 1 then
        begin
        token := charconst;
        if stringpos = 0 then
          intvalue := 0
        else
          intvalue := ord (stringbuf[sharedPtr^.curstringbuf, 1])
        end
      else
        begin
        token := stringconst;
        len := stringpos;
        pos := - 1 {don't dump yet}
        end
  end;
  {>>>}
  {<<<}
  procedure charliteral;
  { Process a character constant of the form #123. }

  var
    val: integer;
    numbers: set of char;
    ok: boolean;

    {<<<}
    procedure snumber;
    { Read a number. }

    begin
      val := 0;
      while nextch in numbers do
        begin
        val := val * 10 + (ord(nextch) - ord('0'));
        getch;
        end;
    end;
    {>>>}

  begin
    numbers := ['0'..'9'];
    ok := false;
    if nextch in numbers then
      begin
      snumber;
      if val <= 255 then
        with tokenSharedPtr^.nexttoken do {check length and set returned token}
          begin
          token := charconst;
          intvalue := val;
          ok := true;
          end;
      end;

    if not ok then
      warnat (badconsterr, sharedPtr^.lastline, chpos);

    getch;
  end;
  {>>>}
  {<<<}
  procedure scannerdirective (l: integer; c: columnindex);
  { Directives addressed to the scanner and listing pass may be included
    in the source text.  They are flagged by a percent sign (%), and have
    no direct effect on the pascal code.
    The only important scanner directive is "%INCLUDE", which causes the
    inclusion of other source files in the source.

    Read and process a scanner directive.
    The only directives accepted are "include" and "page", and only "include" has any effect on the scanner.
    An include directive is expected to be followed by a file name, delimited
    by a blank or semicolon.  When such a file has been found, the source
    stack is pushed and the file specified opened as the next source file
  }
  var
    s: alfa;                { directive as scanned}
    i: identifierrange;     { general use induction var}
    filenamebuilt: boolean; { true if filename correctly built}
    newform: boolean;       { true if new type %include (quoted filename)}
    oldfilename_length: filenameindex; { for saving filename_length in stack}
    newfilename_length: filenameindex; { to temporarily save new filename_length}

  begin
    if (sharedPtr^.switchcounters[standard] > 0) then
      warnat (badchar, l, c);

    i := 0;
    s := '          ';

    getch;
    while ch in ['A'..'Z', 'a'..'z'] do
      begin {read the directive}
      if i < 10 then
        begin
        i := i + 1;
        if ch in ['A'..'Z'] then
          s[i] := chr(ord(nextch) + lowercase)
        else
          s[i] := ch;
        end;
      getch;
      end;

    { Now see if the directive is known, and handle if necessary }
    if s = 'include   ' then
      begin {open a new level of source}
      if sharedPtr^.sourcelevel = sourcedepth then
        scanFatal (deepinclude);

      while nextch in [' ', chr(tabch)] do
        getch;

      filenamebuilt := false;
      i := 0;
      newform := nextch = '''';
      if newform then
        begin
        getch;
        while (nextch <> '''') and not endofline do
          begin
          if i < filenamelen then
            begin
            i := i + 1;
            sharedPtr^.filename[i] := nextch;
            end;
          getch;
          end;
        if nextch = '''' then
          begin
          getch;
          filenamebuilt := true;
          end
        else
          warnat (longstring, sharedPtr^.lastline - 1, chpos);
        end
      else
        begin
        while not (nextch in [' ', chr(tabch), ';']) do
          begin
          if i < filenamelen then
            begin
            i := i + 1;
            sharedPtr^.filename[i] := nextch;
            end;
          getch;
          end;
        filenamebuilt := true;
        getch;
        end;

      oldfilename_length := sharedPtr^.filename_length;
      newfilename_length := i;
      while i < filenamelen do
        begin
        i := i + 1;
        sharedPtr^.filename[i] := ' '
        end;

      while (nextch in [' ', chr(tabch)]) and not endofline do
        getch;
      if nextch = ';' then
        getch;

      if filenamebuilt then
        begin
        sharedPtr^.sourcelevel := sharedPtr^.sourcelevel + 1;
        openSource;
        with saveinput[sharedPtr^.sourcelevel] do
          begin
          savech := nextch;
          savepos := linepos;
          saveendofline := endofline;
          savefileindex := curFileIndex; { stringtable index of filename}
          savefilename_length := oldfilename_length;
          savebuf := currentbuf;
          linepos := inputbufsize;
          end;

        sharedPtr^.filename_length := newfilename_length;
        logInputFilename;
        baseline[sharedPtr^.sourcelevel] := sharedPtr^.lastline - tokenSharedPtr^.nexttoken.baseline - ord(endofline);
        if not endofline then
          sharedPtr^.lastline := sharedPtr^.lastline + 1;
        tokenSharedPtr^.nexttoken.baseline := sharedPtr^.lastline - 1;

        endofline := true;
        end;
      end

    else if s <> 'page      ' then
      begin
      warnat (baddirective, l, c);
      getch
      end;

    ch := ' ';
    nextch := ' ';
  end;
  {>>>}
  {<<<}
  procedure number;
  { Convert number tokens
    Real numbers have token "realconstant" or "dblrealconst", and the actual value is returned in "realvalue".
    Integers, of whatever base, have the token "intconstant", and the value is left in "intvalue"
  }

  {<<<}
  const
    binbase = 256; {binary base for conversions}
    maxproduct = 4095; {maximum base * binbase - 1}

    bytesize = 256; {number of elements in target byte}
    maxbytevalue = 255; {greatest single value in byte}
    maxinbuf = linelen; {buffer up to entire line length of digits}
  {>>>}
  {<<<}
  type
    digitindex = 0..maxinbuf;
    byte = 0..maxbytevalue;
  {>>>}
  {<<<}
  var
    digits: array [digitindex] of byte; {actual digits read}
    length: digitindex; {length of number read}
    leadingzeros: digitindex; {number of leading zeros (for real fraction)}
    fill: digitindex; {index used to fill the digit array}
    draw: digitindex; {index used to extract ch values from the digit array}
    i: digitindex; {induction}
    radix: integer; {controls conversion radix for integers}

    realresult: realarray; {receives result of call to real conversion routine}
    realerror: realstatus; {returns real conversion error code}
    realmode: realmodetype; {real number conversion control code}
    j: 1..maxrealwords; {induction}
    isdouble: boolean; {true if p2rdreal scans 'D' in real}
  {>>>}

    {<<<}
    procedure numbererror (err: warning);
    { Issue a warning message with a pointer in the middle of the number }

    begin
      with tokenSharedPtr^.nexttoken do
        if sharedPtr^.lastline = line then
          warnat (err, line, (left + chpos - 1) div 2)
        else
          warnat (err, sharedPtr^.lastline, (2 + right) div 2)
    end;
    {>>>}
    {<<<}
    procedure readReal (function nextch (firstch: boolean): char;
                        var result: realarray; var errorcode: realstatus;
                        mode: realmodetype; var isdouble: boolean);
    {<<<}
    label
      1;
    {>>>}
    {<<<}
    const
      MaxInBuf = 804; { to support IEEE double with denormals }

    {
      == (leading binary zeroes) + (precision+1) - (leading decimal zeroes)

      DECsingle   ==      127     +  25   -   38  ==        114
      DECdouble   ==      127     +  57   -   38  ==        146

      IEEEsingle  == ( 126 + 23)  +  25   -   45  ==        129
      IEEEdouble  == (1022 + 52)  +  54   -  323  ==        805

        buffer size for formats with 15-bit exponent fields :

      DECquad(H)  ==      16383    + 114  - 4932  ==      11565

      IEEEextend  == (16382 +  63) +  65  - 4950  ==      11560
      IEEEquad    == (16382 + 111) + 113  - 4964  ==      11642
    }

      decbase = 10; { decimal radix }
      binbase = 256; { binary radix }
      maxproduct = 2559; { decbase * binbase - 1 }

      MaxExpon = 9999; { maximum exponent accepted during input scan }
      ExpLimit = 999; { (MaxExpon - 9) div 10 }
    {>>>}
    {<<<}
    type

      BufferIndex = 0..MaxInBuf;
      DecExpType = - MaxExpon..MaxExpon;
      BinExpType = - halfmaxword..halfmaxword;
    {>>>}
    {<<<}
    var
      digits: packed array [BufferIndex] of byte;
      value: realarray; { local copy of final binary result }

      format: RealFormat; { }
      precision: RealPrecision; { }
      rounding: RealRounding; { }

      realbits: 1..maxrealbits; { total number of binary bits of significance }
      realbytes: 1..maxrealbytes; { number of bytes of significance }
      realwords: 1..maxrealwords; { number of words in target real }

      impliedbit: 0..1; { 1 if binary format uses "hidden bit" }
      pointoffset: 0..bitsperbyte; { 1 for IEEEformat, 0 for others }

      exponoffset: 0..bitsperbyte; { exponent offset within high byte }

      ch: char; { last char returned from "nextch" function }

      ExpValue: DecExpType; { value which follows "E" during scan }
      DecExp: DecExpType; { value of decimal exponent after scan }

      MaxDecExp: DecExpType; { maximum decimal exponent value }
      MinDecExp: DecExpType; { minimum decimal exponent value }

      MaxBinExp: BinExpType; { maximum unbiased exponent value }
      MinBinExp: BinExpType; { minimum unbiased exponent value }

      ExpBias: BinExpType; { exponent bias for this format }
      BinExp: BinExpType; { final unbiased binary exponent }

      NeedBits: BinExpType; { binary bits needed to finish conversion }

      ZeroBits: 0..bitsperbyte; { leading zero bits in high order byte }
      DenormBits: BinExpType; { number of bits to denormalize result by }

      class: RealClass; { class of resultant operand }

      negative: boolean; { true if minus sign appeared in input string }
      signed: boolean; { true if minus sign or plus sign encountered }
      expneg: boolean; { true if exponent sign is negative }
      StickyBit: boolean; { true if nonzero digits beyond buffer capacity }
      RoundBit: boolean; { true if the next bit past the LSB is non-zero }
      RoundUp: boolean; { true if rounding is applied to the fraction }
      Inexact: boolean; { true if the conversion is not exact }
      reversewds: boolean; { true if result is stored low word to hi word }
      denormalizing: boolean; { true if result may be gradually denormalized }

      product: 0..maxproduct;
      shifter: word;

      power: byte;
      carry: byte;
      temp: byte;

      dpt: BufferIndex; { index into digits[] when scanning input }
      i: BufferIndex; { temporary induction var }
      j: BufferIndex; { temporary induction var }

     { indices which delimit the integer and fraction parts of digits[] buffer }

      inthead: BufferIndex;
      inttail: BufferIndex;
      frachead: BufferIndex;
      fractail: BufferIndex;
    {>>>}

    {<<<}
    procedure getch;

    begin
      ch := nextch(false);
    end;
    {>>>}
    {<<<}
    function alfanumeric (ch: char): boolean;


      begin
        alfanumeric := (ch >= 'A') and (ch <= 'Z') or (ch >= 'a') and
                       (ch <= 'z') or (ch >= '0') and (ch <= '9');
      end { alfanumeric } ;
    {>>>}
    {<<<}
    function uppercase (ch: char): char;


      begin
        if (ch >= 'a') and (ch <= 'z') then
          uppercase := chr(ord(ch) - ord('a') + ord('A'))
        else uppercase := ch
      end { uppercase } ;
    {>>>}
    {<<<}
    function chEquals (c: char): boolean;


      begin
        if (uppercase(ch) = c) then
          begin
          getch;
          chEquals := true
          end
        else chEquals := false
      end { chEquals } ;
    {>>>}
    {<<<}
      procedure GetFractionByte (NeedBits: BinExpType);

    { Maintains the 2 indices, "frachead" and "fractail", which delimit the
      fraction string, and "StickyBit", which records the state of digits to
      the right of "fractail", which can no longer directly affect the result.
      "NeedBits" is the computed maximum number of decimal digits which are
      required to complete the binary conversion.  The output is one byte of
      binary stored in "carry".  This routine relocates the fraction string to
      the high end of the digit buffer.
    }

    var
      src, dst: BufferIndex; { used to relocate the fraction string }

    begin
      while (digits[fractail] = 0) and (frachead <= fractail) do { zero
              suppress from right }
        fractail := fractail - 1;

      carry := 0;
      if (frachead + NeedBits <= fractail) then
        begin { adjust tail }
        fractail := frachead + NeedBits - 1;
        StickyBit := true { truncating non-zero digits }
        end; { adjust tail }

      if (fractail >= frachead) then
        begin
        dst := MaxInBuf;
        for src := fractail downto frachead do
          begin
          product := digits[src] * binbase + carry;
          digits[dst] := product mod decbase;
          carry := product div decbase;
          dst := dst - 1;
          end;

        frachead := dst + 1;
        fractail := MaxInBuf;
        end;

    end;
    {>>>}
    {<<<}
    procedure initialize;

    var
      i: integer;

    begin
    case (format) of
      IEEEformat1,
      INTELformat1:
        {<<<}
        begin
        pointoffset := 1;
        denormalizing := true;
        if (format = INTELformat1) then
          reversewds := true;

        case (precision) of
          SinglePrecision1:
            begin
            realwords := 2;
            realbytes := 3;
            realbits := 24;

            ExpBias := 127;
            MaxBinExp := + 127;
            MinBinExp := - 126;
            MaxDecExp := + 39;
            MinDecExp := - 45;
            end { SinglePrecision } ;

          DoublePrecision1:
            begin
            realwords := 4;
            realbytes := 7;
            realbits := 53;
            exponoffset := 4;

            ExpBias := 1023;
            MaxBinExp := + 1023;
            MinBinExp := - 1022;
            MaxDecExp := + 309;
            MinDecExp := - 323;
            end { DoublePrecision } ;
          end { case precision } ;
        end { IEEEformat } ;
        {>>>}

      DECformat1:
        {<<<}
        begin
        pointoffset := 0;
        ExpBias := 128;
        MaxDecExp := + 39;
        MinDecExp := - 39;
        MaxBinExp := + 127;
        MinBinExp := - 127;

        case (precision) of
          SinglePrecision1:
            {<<<}
            begin
            realwords := 2;
            realbytes := 3;
            realbits := 24;
            end { SinglePrecision } ;
            {>>>}
          DoublePrecision1:
            {<<<}
            begin
            realwords := 4;
            realbytes := 7;
            realbits := 56;
            end { SinglePrecision } ;
            {>>>}
          end;
        end;
        {>>>}

      IBMformat1:
        {<<<}
        begin
        end { IBMformat } ;
        {>>>}
      end;

    for i := 1 to realwords do
      value[i] := 0;
    end;
    {>>>}

    begin
      isdouble := false;

      { unpack the mode parameter word into constituent parts }
      rounding := loophole(RealRounding, (mode mod nibblesize));
      precision := loophole(RealPrecision, (mode div nibblesize) mod nibblesize);
      format := loophole(RealFormat, (mode div bytesize) mod nibblesize);

      impliedbit := 1; { correct value for most formats }
      exponoffset := 7; { correct value for all single formats }
      reversewds := false; { correct value for all except iAPX86 }
      denormalizing := false; { correct value for non-IEEE formats }

      dpt := 0;
      DecExp := 0;
      BinExp := 0;
      StickyBit := false;
      errorcode := noerror;

      ch := nextch(true);
      while (ch = ' ') do getch;

      negative := (ch = '-');
      signed := negative or (ch = '+');

      if signed then getch;

      if (ch < '0') or (ch > '9') then
        begin
        errorcode := syntaxerr;

        if (format = IEEEformat1) or (format = INTELformat1) then
          begin { permit certain keywords }
          class := SNaNClass { signalling NaN } ;

          if chEquals('I') then
            if chEquals('N') then
              if chEquals('F') then

                begin { scan Infinity syntax }
                { permit alternate spellings }
                if chEquals('I') then
                  if chEquals('N') then
                    if chEquals('I') then
                      if chEquals('T') then if chEquals('Y') then getch;

                { test for proper termination }
                if not alfanumeric(ch) then { Infinity } class := InfClass;
                end { aliases }

              else { ch <> 'F' }
            else { ch <> 'N' }
          else { ch <> 'I' }

          if chEquals('N') then
            if chEquals('A') then
              if chEquals('N') then

                begin { check NaN syntax }
                { permit parenthesized argument }
                if chEquals('(') then while not chEquals(')') do getch;

                { test for proper termination }
                if not alfanumeric(ch) then { quiet NaN } class := QNaNClass
                end { check NaN syntax }

              else { ch <> 'N' }
            else { ch <> 'A' }
          else { ch <> 'N' }
          end { if format = IEEEformat } ;

        goto 1
        end { special operands } ;

      { - - -   scan integer part   - - - }

      while (ch = '0') do getch;

      while (ch >= '0') and (ch <= '9') do
        begin
        if (dpt < MaxInBuf) then
          begin { insert integer digit }
          dpt := dpt + 1;
          digits[dpt] := ord(ch) - ord('0')
          end { insert integer digit }
        else { dpt => MaxInBuf }
          begin { buffer is full }
          DecExp := DecExp + 1;
          if (ch <> '0') then StickyBit := true
          end; { buffer is full }
        getch
        end { while ch is digit } ;

      DecExp := DecExp + dpt;

      { - - -   scan fraction part   - - - }

      if chEquals('.') then
        begin
        if (ch < '0') or (ch > '9') then errorcode := syntaxerr;

        if (dpt = 0) then { no integer part }
          while chEquals('0') do DecExp := DecExp - 1;

        while (ch >= '0') and (ch <= '9') do
          begin
          if (dpt < MaxInBuf) then
            begin { insert fraction digit }
            dpt := dpt + 1;
            digits[dpt] := ord(ch) - ord('0')
            end { insert fraction digit }
          else { dpt => MaxInBuf }
          if (ch <> '0') then StickyBit := true;
          getch
          end { while ch is digit }

        end { scan fraction part } ;

      { - - -   scan exponent part   - - - }

      ch := uppercase(ch);

      if (ch = 'E') or (ch = 'D') then
        begin { exponent scan }
        { It's a double constant, set the flag and precision }
        if ch = 'D' then
          begin
          isdouble := true;
          precision := DoublePrecision1;
          end;

        getch;
        expneg := (ch = '-');
        if expneg or (ch = '+') then getch;

        if (ch < '0') or (ch > '9') then errorcode := syntaxerr;

        ExpValue := 0;
        while (ch >= '0') and (ch <= '9') do
          begin
          if (ExpValue <= ExpLimit) then
            ExpValue := ExpValue * 10 + ord(ch) - ord('0');
          getch
          end;

        if expneg then DecExp := DecExp - ExpValue
        else DecExp := DecExp + ExpValue
        end { exponent scan } ;

      { Now that 'E' or 'D' is passed, we can set the exponent limits }
      initialize;

      {----- scanning is complete; commence conversion -----}
      if (dpt > 0) then
        begin { conversion of non-zero string }
        inthead := 1;

        while (DecExp < dpt) { if a fraction part exists }
              and (digits[dpt] = 0) do
          dpt := dpt - 1; { then delete trailing zeroes }

        if (DecExp > 0) then
          begin { convert integer portion }

          if (DecExp > MaxDecExp) then
            begin { obvious exponent overflow }
            errorcode := overflowerr;
            DecExp := MaxDecExp;
            digits[1] := ord('9');
            end;

          while (DecExp > dpt) do
            begin { append trailing zeroes }
            dpt := dpt + 1;
            digits[dpt] := 0
            end; { append trailing zeroes }

          inttail := DecExp;
          frachead := DecExp + 1;
          fractail := dpt;

          { perform the integer conversion }
          for i := inthead to inttail - 1 do
            begin
            carry := digits[i + 1];

            for j := i downto inthead do
              begin
              product := digits[j] * decbase + carry;
              digits[j + 1] := product mod binbase;
              carry := product div binbase;
              end { for j } ;

            if carry = 0 then inthead := inthead + 1
            else digits[inthead] := carry;
            end { for i } ;

          BinExp := (inttail - inthead + 1) * bitsperbyte;
          end { convert integer portion }

        else { DecExp <= 0 }
          begin { the value has no integer part }

          if (DecExp < MinDecExp) then
            begin { obvious exponent underflow }
            errorcode := underflowerr;
            goto 1
            end;

          frachead := 1;
          fractail := dpt;
          BinExp := bitsperbyte;

          while (DecExp <= 0) do
            begin { fraction scaling }
            { It may not be necessary for all of the decimal fraction digits
            to take part in the conversion -- the exact number of decimal
            digits needed is equivalent to the number of bits in the binary
            result INCLUDING leading zero bits (plus a few for rounding).
            Rather than multiply the decimal exponent by log(2)10 (3.3219...)
            to compute leading binary zeroes, a simpler approximation of 3 3/8
            (3.375) is used, taking care to avoid overflow on 16 bit hosts, even
            when developing extended format reals with 15 bit exponent values.
            }
            NeedBits := - DecExp - DecExp - DecExp; { 3 * abs(DecExp) }
            GetFractionByte(NeedBits div 8 + NeedBits + realbits + 3);
            BinExp := BinExp - bitsperbyte; { adjust binary exponent }

            while (DecExp <> 0) and (carry <> 0) do
              begin
              frachead := frachead - 1;
              digits[frachead] := carry mod decbase;
              carry := carry div decbase;
              DecExp := DecExp + 1;
              end;

            if (carry <> 0) then { force termination of scaling }
              DecExp := DecExp + 1;
            end; { fraction scaling }

          { store first converted binary byte in low order end of buffer }
          digits[1] := carry;
          inttail := 1;
          end { DecExp <= 0 } ;

        { The number now has at least one byte of converted binary integer.
          Truncate the binary integer if there are excess bytes, or call
          GetFractionBytes for additional bytes if there are too few.  Also,
          count the number of significant bits in the high order byte.
        }
        ZeroBits := bitsperbyte; { number of leading zero bits }
        temp := digits[inthead]; { the high order byte }

        repeat
          ZeroBits := ZeroBits - 1;
          temp := temp div 2
        until temp = 0;

        BinExp := BinExp - ZeroBits;
        { BinExp is now the correct unbiased binary exponent }

        if (inttail - inthead >= realbytes) then
          begin { truncate excess bytes }
          inttail := inthead + realbytes;
          frachead := inttail + 1;
          end { truncate excess bytes }

        else { inttail - inthead < realbytes }
          begin { generate additional bytes }
          NeedBits := (realbytes - (inttail - inthead) - 1) * bitsperbyte + ZeroBits + 1;
          repeat
            GetFractionByte(NeedBits);
            inttail := inttail + 1;
            digits[inttail] := carry;
            NeedBits := NeedBits - bitsperbyte;
          until (NeedBits <= 0);
          end; { generate additional bytes }

        while (not StickyBit) and (frachead <= fractail) do
          if (digits[fractail] = 0) then fractail := fractail - 1
          else StickyBit := true;

        { the binary significand is now a (realbytes + 1) byte string
          starting at digits[inthead] and ending at digits[inttail].
          "StickyBit" is true if the conversion is inexact thus far.
        }
        {-------- normalize or denormalize --------}
        DenormBits := 0;
        if (BinExp <= MinBinExp) then
          begin
          errorcode := underflowerr;
          DenormBits := MinBinExp - BinExp;
          BinExp := MinBinExp;
          impliedbit := 0;

          if (not denormalizing) then
            begin
            for i := inthead to inthead + realbytes do digits[i] := 0;
            goto 1
            end
          end;

        { compute bit offset multiplier }
        shifter := (realbits + ZeroBits - DenormBits) mod bitsperbyte;
        power := 1;
        for i := 1 to shifter do power := power + power;

        { compute byte offset index }
        temp := (bitsperbyte - 1) - (realbits - 1) mod bitsperbyte;
        i := inttail - (DenormBits + temp) div bitsperbyte;

        { test the need for an extra right shift }
        if (ZeroBits < shifter) then i := i - 1;

        j := i;

        { scan any bytes being discarded from tail end }
        while (j < inttail) do
          begin
          j := j + 1;
          if (digits[j] <> 0) then StickyBit := true;
          end { while } ;

        { simultaneously shift bits left and bytes right }
        carry := 0;
        while (i >= inthead) do
          begin
          shifter := digits[i] * power + carry;
          digits[j] := shifter mod binbase;
          carry := shifter div binbase;
          i := i - 1;
          j := j - 1;
          end { while } ;

        { flush out remainder of high order, if any }

        while (j >= inthead) do
          begin
          digits[j] := carry;
          carry := 0;
          j := j - 1;
          end { while } ;

        {-------- test for rounding --------}
        RoundBit := (digits[inttail] >= (binbase div 2));

        if (not StickyBit) then
          StickyBit := (digits[inttail] mod (binbase div 2) <> 0);

        Inexact := (RoundBit) or (StickyBit);

        case (rounding) of
          ToNearest:
            RoundUp := (RoundBit) and (StickyBit or odd(digits[inttail - 1]));
          ToZero: RoundUp := false;

          ToPosInf: RoundUp := (not negative) and (Inexact);

          ToNegInf: RoundUp := (negative) and (Inexact);
          end { case (rounding) } ;

        {-------- apply rounding --------}
        if (RoundUp) then
          begin
          carry := 1;
          i := inttail - 1;

          while (carry <> 0) and (i > inthead) do
            begin
            if (digits[i] = binbase - 1) then digits[i] := 0
            else
              begin
              digits[i] := digits[i] + 1;
              carry := 0;
              end;
            i := i - 1;
            end;

          if (carry <> 0) then
            begin { round up high order byte }
            temp := 0;
            for i := 1 to ((realbits - 1) mod bitsperbyte + 1) do
              temp := temp + temp + 1;
            if (digits[inthead] = temp) then
              begin
              digits[inthead] := (temp div 2) + 1;
              BinExp := BinExp + 1;
              end
            else
              begin
              digits[inthead] := digits[inthead] + 1;
              carry := 0;
              end;
            end; { round up high order byte }
          end { rounding } ;

        {-------- test for exponent range error --------}
        if (BinExp - pointoffset > MaxBinExp) then
          begin
          errorcode := overflowerr;
          BinExp := MaxBinExp;
          if (format = IEEEformat1) or (format = INTELformat1) then
            begin
            BinExp := BinExp + 1;
            temp := 0
            end
          else temp := maxbyte;
          for i := inthead to inthead + realbytes do digits[i] := temp;
          goto 1;
          end;

        {-------- packing --------}
        shifter := (BinExp + ExpBias - pointoffset - impliedbit);
        for i := 1 to exponoffset do shifter := shifter + shifter; { shift
          exponent left }
        value[1] := shifter + digits[inthead];

        j := inthead;
        for i := 2 to realwords do
          begin
          j := j + 2;
          value[i] := digits[j - 1] * binbase + digits[j];
          end;

        end {of non-zero value } ;

      {---- append sign and deliver result ----}
    1:
      if negative then value[1] := value[1] + halfwordsize;

      if (reversewds) then
        begin
        j := realwords;
        for i := 1 to realwords do
          begin
          result[i] := value[j];
          j := j - 1;
          end;
        end
      else { not reverse words }
        for i := 1 to realwords do result[i] := value[i];

    end;
    {>>>}
    {<<<}
    procedure readdigits (skipzeros: boolean {skip and count leading zeros } );
    {<<<}
    { Read digits into the digit array.  If "skipzeros" is true, then leading
      zeros are not significant.  They are counted in "leadingzeros" rather
      than inserted into the digit array.  Otherwise, all digit characters are
      converted to numerics and inserted into "digits" beginning at "fill" + 1.
    }
    {>>>}

      var
        digit: 0..15; {buffers the numeric value of "ch"}
        scanning: boolean;


      begin {readdigits}
        scanning := true;
        leadingzeros := 0;
        length := 0;

        while skipzeros and (ch = '0') do
          begin {scan leading zeros}
          leadingzeros := leadingzeros + 1;
          getch;
          end;

        while scanning do
          begin
          if (ch >= '0') and (ch <= '9') then digit := ord(ch) - ord('0')
          else if (ch >= 'a') and (ch <= 'f') then
            digit := ord(ch) - ord('a') + 10
          else scanning := false;

          if scanning and (digit < radix) then
            begin
            length := length + 1;
            fill := fill + 1;
            digits[fill] := digit;
            getch;
            end
          else scanning := false;
          end {while} ;

        if length + leadingzeros = 0 then numbererror(missingdigits);
      end {readdigits} ;
    {>>>}
    {<<<}
    procedure convertinteger (var value1: integer);
    { Interpret the digits according to the current radix, and return the value }

    type
      intarray = array [1..maxintarray] of integer;

    var
      valrec:
        record
          case boolean of
            false: (val: intarray); {used to loophole long integers}
            true: (valint: integer);
        end;

      i, j, head: digitindex;
      carry: byte;
      product: 0..maxproduct;
      digit: integer;

    begin
      head := 1;

      for i := 1 to length - 1 do
        begin
        carry := digits[i + 1];

        for j := i downto head do
          begin
          product := digits[j] * radix + carry;
          digits[j + 1] := product mod binbase;
          carry := product div binbase;
          end {for j} ;

        if carry = 0 then head := head + 1
        else digits[head] := carry;
        end {for i} ;

      if (length - head + 1 > sharedPtr^.targetintsize) or
         (length - head + 1 = sharedPtr^.targetintsize) and
         (sharedPtr^.switchcounters[standard] > 0) and
         (digits[head] >= bytesize div 2) then

        begin {fabricate a multi-precision "maxint"}
        valrec.val[1] := maxint;
        for i := 2 to maxintarray do
          valrec.val[i] := maxusint;
        numbererror (badinteger)
        end {integer error processing}

      else
        begin
        { transfer }
        j := 0; { now an index into "valrec.val" }
        digit := 0; {accumulates one host integer}

        for i := sharedPtr^.targetintsize - 1 downto 0 do
          begin

          if length - i >= head then {we've come to a byte to xfer}
            digit := (digit * 256) or digits[length - i];

          if i mod hostintsize = 0 then
            begin {move one host integer into target intarray}
            j := j + 1;
            valrec.val[j] := digit;
            digit := 0;
            end;

          end {for i} ;
        end {transfer} ;

      value1 := valrec.valint;
    end;
    {>>>}
    {<<<}
    function getrealch (first: boolean): char;
    { returns a character from the current number. This number may have been buffered partly in the array 'digits' }

    begin
      if first then
        draw := 0;

      if leadingzeros <> 0 then
        begin
        getrealch := '0';
        leadingzeros := 0;
        end
      else if draw < fill then
        begin
        draw := draw + 1;
        getrealch := chr(digits[draw] + ord('0'));
        end
      else
        begin
        if draw > fill then
          getch
        else
          draw := draw + 1;
        getrealch := ch;
        end;
    end;
    {>>>}

  begin
    { fill digit array, skip leading zeros }
    fill := 0;
    radix := 10;
    readdigits (true);

    with tokenSharedPtr^.nexttoken do
      if ((ch = '.') and (nextch <> '.') and (nextch <> ')')) or (ch = 'e') or (ch = 'd') then
        {<<<  parse real}
        begin
        token := realconst;
        realmode := IEEEformat;

        if sharedPtr^.switcheverplus[doublereals] then
          realmode := realmode + doubleprecision
        else
          realmode := realmode + singleprecision;

        for j := 1 to maxrealwords do
          realresult[j] := 0; {clear the result first}

        readreal (getrealch, realresult, realerror, realmode, isdouble);
        realvalue := realresult;
        first_real_seen := true; {to detect $double error}

        case realerror of
          noerror: ;
          syntaxerr:
            numbererror (missingdigits);
          underflowerr,
          overflowerr:
            numbererror (badexpon);
          end;

        if isdouble then
          if sharedPtr^.switchcounters[standard] <= 0 then
            token := dblrealconst
          else
            numbererror (badconsterr);
        end
        {>>>}
      else
        {<<<  parse integer}
        begin
        token := intconst;
        if ch = 'b' then
          begin
          if (sharedPtr^.switchcounters[standard] > 0) then
            numbererror (octalconst);

          getch;
          radix := 8;
          for i := 1 to length do {check for 8's or 9's}
            if digits[i] >= 8 then
              warnat(badoctal, sharedPtr^.lastline, chpos + i - length - 2);
          convertinteger (intvalue);
          end {octal 'b' form}

        else if ch = '#' then
          begin
          if (sharedPtr^.switchcounters[standard] > 0) then
            numbererror (nondecimalconst); {"...not standard Pascal"}

          convertinteger (radix);
          if (radix < 2) or (radix > 16) then
            begin
            radix := 16;
            numbererror (badradix); {"...must lie in range 2..16"}
            end;

          getch;
          fill := 0;
          readdigits (true);
          convertinteger (intvalue);
          end {non-decimal integer}
        else
          convertinteger (intvalue);

        if ch in ['a'..'z'] then {as in "10div"}
          numbererror (badnumber);
        end;
        {>>>}
  end;
  {>>>}
  {<<<}
  procedure identifier;
  { Scan an identifier or reserved word.
    The identifier text is stored in the string table, and a hash function computed as it is scanned.
    If the identifier is not a reserved word, it is checked in the hash table and inserted if not found.
    The hash function used was determined empirically from actual Pascal programs
  }
  var
    t1, t2: 0..maxusword;   { used in computing hash function}
    lastch: integer;        { position of end of id in stringtable}
    count: identifierrange; { identifier length}
    hash: hashindex;        { location within hash table}
    w: reswordtype;         { first part of id for reserved word check}

    {<<<}
    procedure maptoken (var token: tokentype);
    { Check the identifier just scanned against the reserved word table.
      Token is set to "ident" if not found, or the the corresponding token
      if this is a reserved word.

      The reserved word table is sorted by identifier length and alphabetical
      order, and a binary search is done on the portion of the table with the
      length of the identifier just read.
    }
    var
      left, right, middle: reservedindex; {binary search pointers}

    begin
      token := ident;
      if (count >= minreslen) and (count <= maxreslen) then
        begin
        left := reslentable[count];
        right := reslentable[count + 1] - 1;
        while left < right do
          begin
          middle := (left + right) div 2;
          if reswords[middle] < w then left := middle + 1
          else right := middle;
          end;
        if reswords[right] = w then token := reswordtokens[right]
        end
    end;
    {>>>}
    {<<<}
    function identFound: boolean;
    { Check the identifier just read against the hash table and return "true" if it is already there }

    var
      i, j: integer;

    begin
      identFound := false;
      with hashtable[hash] do
        if len = count then
          begin
          j := pos;
          i := sharedPtr^.stringtabletop + 1;
          while (sharedPtr^.stringtable^[i] = sharedPtr^.stringtable^[j]) and (i <= lastch) do
            begin
            i := i + 1;
            j := j + 1
            end;
          if i > lastch then
            identFound := true;
          end;
    end;
    {>>>}

  begin
    if sharedPtr^.switcheverplus[caseswitch] then
      begin
      convertingcase := false;
      ch := unconvertedch;
      end;

    w := '         ';
    count := 0;
    lastch := sharedPtr^.stringtabletop;

    t2 := 0;
    repeat
      {<<<  parse identifier in to w, forminh hash t2}
      count := count + 1;
      if count <= maxreslen then
        begin
        w[count] := ch;

        { form hash }
        t1 := ord(ch) mod 32;
        case (count - 1) mod 3 of
          0: t2 := t2 + t1;
          1: t2 := t2 + t1 * 32;
          2: t2 := t2 + t1 * 1024;
          end;
        end;

      if lastch < stringtablesize then
        begin
        lastch := lastch + 1;
        sharedPtr^.stringtable^[lastch] := ch;
        end
      else
        scanFatal (stringtableoverflow);

      getch;
      if (sharedPtr^.switchcounters[standard] > 0) and (ch in ['$', '_']) then
        warnat (badchar, sharedPtr^.lastline, chpos);
      until not (ch in ['$', '_', 'A'..'Z', 'a'..'z', '0'..'9']);
      {>>>}

    with tokenSharedPtr^.nexttoken do
      begin
      maptoken (token);
      if token = ident then
        begin
        hash := t2 mod hashtablesize;
        while (hashtable[hash].pos <> 0) and not (identFound or sharedPtr^.fatalflag) do
          hash := (hash + count) mod hashtablesize;
        with hashtable[hash] do
          if pos = 0 then
            begin
            sharedPtr^.insertions := sharedPtr^.insertions + 1;
            if sharedPtr^.insertions = hashtablesize then
              scanFatal (tablefull);
            pos := sharedPtr^.stringtabletop + 1;
            len := count;
            key := sharedPtr^.insertions;
            sharedPtr^.stringtabletop := lastch
            end;
        key := hashtable[hash].key;
        keypos := hashtable[hash].pos;
        end
      end;

    if sharedPtr^.switcheverplus[caseswitch] then
      convertingcase := true;
  end;
  {>>>}

begin
  somethingelse := false;

  if nextswitchread then
    begin
    sharedPtr^.switchcounters := nextswitchcounters;
    sharedPtr^.switcheverplus := nextswitcheverplus;
    nextswitchread := false;
    end;

  while not (somethingelse or endofinput) do
    case ch of
      ' ':
        {<<<}
        begin
        skippingblanks := true;
        getch;
        skippingblanks := false;
        end;
        {>>>}
      '{':
        {<<<}
        begin
        commentch := '}';
        skipcomment
        end;
        {>>>}
      '(':
        {<<<}
        begin
        if nextch = '*' then
          begin
          commentch := ')';
          getch;
          skipcomment
          end
        else
          somethingelse := true;
        end;
        {>>>}
      '/':
        {<<<}
        begin
        if nextch = '*' then
          begin
          warnat(obsoletecomments, sharedPtr^.lastline, chpos);
          commentch := '/';
          getch;
          skipcomment;
          end
        else
          somethingelse := true;
        end;
        {>>>}
      '%':
        scannerdirective (sharedPtr^.lastline, chpos);
      '$', '''', ')', '*', '+', ',', '-', '.', ':', ';', '<', '=', '>', '@',
      '#', '[', ']', '^', '_', '0', '1', '2', '3', '4', '5', '6', '7', '8',
      '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a',
      'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
      'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z':
        somethingelse := true;
      otherwise
        {<<<}
        begin
        if ch = chr(cr) then
          getch
        else
          begin
          warnat (badchar, sharedPtr^.lastline, chpos);
          getch;
          end;
        end;
        {>>>}
      end;

  if not first_token_seen and nextswitchread then
    {<<<  set embedded switches prior to the first token}
    { important for the 68000, 68020 and 68881 embedded switches because of the processor type check that is below }

    begin
      sharedPtr^.switchcounters := nextswitchcounters;
      sharedPtr^.switcheverplus := nextswitcheverplus;
      nextswitchread := false;
    end;
    {>>>}

  first_token_seen := true;
  with tokenSharedPtr^.nexttoken do
    begin
    line := sharedPtr^.lastline;
    left := chpos;
    filepos := current_filepos;

    if (endofinput or sharedPtr^.fatalflag) then
      token := eofsym
    else
      case ch of
        '.':
          {<<<}
          begin
          if nextch = '.' then
            begin
            token := dotdot;
            getch;
            end

          else if nextch = ')' then
            begin
            token := rbrack;
            getch;
            end

          else
            token := dot;

          getch;
          end;
          {>>>}
        '<':
          {<<<}
          begin
          if nextch = '=' then
            begin
            token := leq;
            getch;
            end

          else if nextch = '>' then
            begin
            token := neq;
            getch;
            end

          else
            token := lss;

          getch;
          end;
          {>>>}
        '>':
          {<<<}
          begin
          if nextch = '=' then
            begin
            token := geq;
            getch;
            end
          else
            token := gtr;

          getch;
          end;
          {>>>}
        ':':
          {<<<}
          begin
          if nextch = '=' then
            begin
            token := becomes;
            getch;
            end
          else
            token := colon;

          getch;
          end;
          {>>>}
        '(':
          {<<<}
          begin
          if nextch = '.' then
            begin
            token := lbrack;
            getch;
            end
          else
            token := lpar;

          getch;
          end;
          {>>>}
        ')', '*', '+', ',', '-', '/', ';', '=', '[', ']', '@', '^':
          {<<<}
          begin
          token := tokentable[ch];

          getch;
          end;
          {>>>}
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
          number;
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z':
          identifier;
         '$', '_':
           {<<<}
           begin
           if (sharedPtr^.switchcounters[standard] > 0) then
             warnat (badchar, sharedPtr^.lastline, chpos);

           identifier;
           end;
           {>>>}
        '''', '"':
          stringliteral (ch);
        '#':
          {<<<}
          begin
          if (targetopsys <> msdos) or (sharedPtr^.switchcounters[standard] > 0) then
            warnat (badchar, sharedPtr^.lastline, chpos);

          charliteral;
          end;
          {>>>}
        otherwise
          {<<<}
          begin
          warnat (badchar, sharedPtr^.lastline, chpos);

          getch;
          end
          {>>>}
        end;

    if not endofline and (sharedPtr^.lastline > line) then
      begin
      left := 1;
      line := sharedPtr^.lastline
      end;

    right := oldchpos;
    fileindex := curFileIndex; { stringtable index of filename}
    end;

  puttokenfile;
end;
{>>>}

{<<<}
procedure scan1;
{ Init scanner, read first char, etc. }

  {<<<}
  procedure initscanner;
  {<<<}
  {    Enterstandardid -- builds entries in hashtable and string-}
  {      table for each standard identifier (i.e. true,false).   }
  {    Enterresword -- builds entries in reswords and            }
  {      and reswordtokens for tokens like BEGIN, END.           }
  {    Initscanswitches -- set up switch name table              }
  {    Inittokentable -- set up tokentable for all one letter    }
  {      tokens.                                                 }
  {    Initreswords -- enter all reserved words, also sets       }
  {      up reslentable                                          }
  {    Initstandardids -- uses enterstandardid to enter all      }
  {      predefined identifiers.                                 }
  {>>>}

  var
    resindex: 0..reservedcount; {index in reslentable}
    i: hashindex; {general induction}
    t1, t2: 0..maxusword; {hash value temps}

    {<<<}
    procedure enterstandardid (n: alfa; newlen: identifierrange; id: standardids);

    var
      hash: hashindex;
      i: identifierrange;

    begin
      sharedPtr^.insertions := sharedPtr^.insertions + 1;
      t2 := 0;
      for i := 1 to newlen do
        begin
        sharedPtr^.stringtable^[sharedPtr^.stringtabletop + i] := n[i];
        t1 := ord(n[i]) mod 32;
        case (i - 1) mod 3 of
          0: t2 := t2 + t1;
          1: t2 := t2 + t1 * 32;
          2: t2 := t2 + t1 * 1024;
          end;
        end;
      hash := t2 mod hashtablesize;
      while hashtable[hash].pos <> 0 do
        hash := (hash + newlen) mod hashtablesize;

      with hashtable[hash] do
        begin
        pos := sharedPtr^.stringtabletop + 1;
        len := newlen;
        key := sharedPtr^.insertions;
        end;

      sharedPtr^.stringtabletop := sharedPtr^.stringtabletop + newlen;
      sharedPtr^.standardidtable[id] := sharedPtr^.insertions;
    end;
    {>>>}
    {<<<}
    procedure enterresword (resword: reswordtype; restoken: tokentype);

    begin
      resindex := resindex + 1;
      reswords[resindex] := resword;
      reswordtokens[resindex] := restoken;
    end;
    {>>>}
    {<<<}
    procedure initscanswitches;

    var
      i: scanswitchindex;

       {<<<}
       procedure initoneswitch(thiss: switch; {switch type}
                               thisn: switchname; {name}
                               thisv: switchvalue {bumpvalue});

          begin {initoneswitch}
          i := i + 1;
          with scanswitchtable[i] do
            begin
            internal := false;
            s := thiss;
            v := thisv;
            n := thisn;
            end;
          end {initoneswitch} ;
       {>>>}

    begin
      i := 0;

      { must put the checking switches at the beginning}
      initoneswitch (rangecheck ,   'rangecheck    ', 1);
      initoneswitch (indexcheck,    'indexcheck    ', 1);
      initoneswitch (nilcheck,      'pointercheck  ', 1);
      initoneswitch (stackcheck,    'stackcheck    ', 1);
      initoneswitch (mathcheck,     'mathcheck     ', 1);
      initoneswitch (noswitch,      'check         ', 1);
      initoneswitch (doublereals,   'double        ', 1);
      initoneswitch (mainbody,      'main          ', 1);
      initoneswitch (own,           'own           ', 1);

      { The walkback, debug, and profile embedded switches have been disabled.
        They are still allowed, for source code compatibility, but they do nothing. }

      initoneswitch (walkback,      'walkback      ', 0);
      { 1 - ord(switcheverplus[profiling] or switcheverplus[debugging] or switcheverplus[sharecode])); }
      initoneswitch (debugging,     'debug         ', 0);
      { ord(switcheverplus[debugging])); }
      initoneswitch (debugging,     'profile       ', 0);
      { ord (switcheverplus[profiling])); }

      if sharedPtr^.switcheverplus[debugging] or sharedPtr^.switcheverplus[profiling] then
        initoneswitch (listcount, 'list          ', 0) {ignore list/nolist}
      else
        initoneswitch (listcount, 'list          ', ord(sharedPtr^.switcheverplus[listcount]));

      initoneswitch (shortintegers, 'shortints     ', 1);
      initoneswitch (fpp,           'fpp           ', 1);
      initoneswitch (oldpacking,    'oldpacking    ', 1);
      initoneswitch (standard,      'standard      ', 1);
      initoneswitch (stmtnumbers,   'stmtnum       ', 1);
      initoneswitch (details,       'details       ', 1);
      initoneswitch (pic,           'pic           ', 1);
      initoneswitch (caseswitch,    'case          ', 1);
      initoneswitch (multidef,      'multidef      ', 1);
      initoneswitch (framepointer,  'framepointer  ', 1);

      initoneswitch (awaremode,     'aware         ', 1);
      initoneswitch (cpu68000,      '68000         ', 1);
      initoneswitch (cpu68020,      '68020         ', 1);
      initoneswitch (fpc68881,      '68881         ', 1);

      { expand original structured constant }
      i := i + 1;
      scanswitchtable[i].n := 'ident         ';
      scanswitchtable[i].internal := true;
      scanswitchtable[i].is = identsw;

      i := i + 1;
      scanswitchtable[i].n := 'module        ';
      scanswitchtable[i].internal := true;
      scanswitchtable[i].is = identsw;

      i := i + 1;
      scanswitchtable[i].n := 'codesect      ';
      scanswitchtable[i].internal := true;
      scanswitchtable[i].is = codesectsw;

      i := i + 1;
      scanswitchtable[i].n := 'section       ';
      scanswitchtable[i].internal := true;
      scanswitchtable[i].is = xsectionsw;

      i := i + 1;
      scanswitchtable[i].n := 'xxshortsection';
      scanswitchtable[i].internal := true;
      scanswitchtable[i].is = xshortsectsw;

      if targetopsys = vdos then
        scanswitchtable[i].n := 'shortsection  ';

      i := i + 1;
      scanswitchtable[i].n := 'version       ';
      scanswitchtable[i].internal := true;
      scanswitchtable[i].is = xversionsw;

      if i > maxscanswitch then
        begin
        write ('scan switches init error');
        abort (inconsistent);
        end;
    end;
    {>>>}
    {<<<}
    procedure inittokentable;

    begin
      tokentable[')'] := rpar;
      tokentable['*'] := star;
      tokentable['+'] := plus;
      tokentable[','] := comma;
      tokentable['-'] := minus;
      tokentable['.'] := dot;
      tokentable['/'] := slash;
      tokentable[':'] := colon;
      tokentable[';'] := semicolon;
      tokentable['<'] := lss;
      tokentable['='] := eql;
      tokentable['>'] := gtr;
      tokentable['['] := lbrack;
      tokentable[']'] := rbrack;
      tokentable['@'] := uparrow;
      tokentable['^'] := uparrow;
    end;
    {>>>}
    {<<<}
    procedure initreswords;

    begin
      reslentable[2] := resindex + 1;

      enterresword ('do       ', dosym);
      enterresword ('if       ', ifsym);
      enterresword ('in       ', insym);
      enterresword ('of       ', ofsym);
      enterresword ('or       ', orsym);
      enterresword ('to       ', tosym);
      reslentable[3] := resindex + 1;

      enterresword ('and      ', andsym);
      enterresword ('div      ', divsym);
      enterresword ('end      ', endsym);
      enterresword ('for      ', forsym);
      enterresword ('mod      ', modsym);
      enterresword ('nil      ', nilsym);
      enterresword ('not      ', notsym);
      enterresword ('set      ', setsym);
      if not (sharedPtr^.switcheverplus[standard] or sharedPtr^.switcheverplus[oldreswords]) then
        enterresword ('use      ', usesym);
      enterresword ('var      ', varsym);
      reslentable[4] := resindex + 1;

      enterresword ('case     ', casesym);
      enterresword ('else     ', elsesym);
      enterresword ('file     ', filesym);
      enterresword ('goto     ', gotosym);
      enterresword ('then     ', thensym);
      enterresword ('type     ', typesym);
      if not sharedPtr^.switcheverplus[standard] then
        enterresword ('univ     ', univsym);
      enterresword ('with     ', withsym);
      reslentable[5] := resindex + 1;

      enterresword ('array    ', arraysym);
      enterresword ('begin    ', beginsym);
      enterresword ('const    ', constsym);
      enterresword ('label    ', labelsym);
      enterresword ('until    ', untilsym);
      enterresword ('while    ', whilesym);
      reslentable[6] := resindex + 1;

      if not (sharedPtr^.switcheverplus[standard] or sharedPtr^.switcheverplus[oldreswords]) then
        enterresword('define   ', definesym);
      enterresword ('downto   ', downtosym);
      if not sharedPtr^.switcheverplus[standard] then
        enterresword('origin   ', originsym);
      enterresword ('packed   ', packedsym);
      enterresword ('record   ', recordsym);
      enterresword ('repeat   ', repeatsym);
      if not (sharedPtr^.switcheverplus[standard] or sharedPtr^.switcheverplus[oldreswords]) then
        enterresword ('shared   ', sharedsym);
      if not sharedPtr^.switcheverplus[standard] then
        enterresword ('string   ', stringsym);
      reslentable[7] := resindex + 1;

      enterresword ('program  ', programsym);
      reslentable[8] := resindex + 1;

      enterresword ('function ', functionsym);
      reslentable[9] := resindex + 1;

      if not sharedPtr^.switcheverplus[standard] then
        enterresword ('otherwise', otherwisesym);
      enterresword ('procedure', proceduresym);
      reslentable[10] := resindex + 1;
    end;
    {>>>}
    {<<<}
    procedure initstandardids;

    begin
      enterstandardid ('integer   ', 7, integerid);
      enterstandardid ('real      ', 4, realid);
      enterstandardid ('double    ', 6, doubleid);
      enterstandardid ('char      ', 4, charid);
      enterstandardid ('boolean   ', 7, booleanid);

      enterstandardid ('true      ', 4, trueid);
      enterstandardid ('false     ', 5, falseid);

      enterstandardid ('text      ', 4, textid);
      enterstandardid ('input     ', 5, inputid);
      enterstandardid ('output    ', 6, outputid);
      enterstandardid ('write     ', 5, writeid);
      enterstandardid ('writeln   ', 7, writelnid);
      enterstandardid ('read      ', 4, readid);
      enterstandardid ('readln    ', 6, readlnid);
      enterstandardid ('get       ', 3, getid);
      enterstandardid ('put       ', 3, putid);
      enterstandardid ('reset     ', 5, resetid);
      enterstandardid ('rewrite   ', 7, rewriteid);
      enterstandardid ('close     ', 5, closeid);

      enterstandardid ('break     ', 5, breakid);

      enterstandardid ('new       ', 3, newid);
      enterstandardid ('dispose   ', 7, disposeid);

      enterstandardid ('pack      ', 4, packid);
      enterstandardid ('unpack    ', 6, unpackid);

      enterstandardid ('abs       ', 3, absid);
      enterstandardid ('sqr       ', 3, sqrid);
      enterstandardid ('sin       ', 3, sinid);
      enterstandardid ('cos       ', 3, cosid);
      enterstandardid ('exp       ', 3, expid);
      enterstandardid ('ln        ', 2, lnid);
      enterstandardid ('sqrt      ', 4, sqrtid);
      enterstandardid ('arctan    ', 6, arctanid);
      enterstandardid ('odd       ', 3, oddid);

      enterstandardid ('eof       ', 3, eofid);
      enterstandardid ('eoln      ', 4, eolnid);
      enterstandardid ('trunc     ', 5, truncid);
      enterstandardid ('round     ', 5, roundid);

      enterstandardid ('sngl      ', 4, snglid);
      enterstandardid ('dbl       ', 3, dblid);
      enterstandardid ('ord       ', 3, ordid);
      enterstandardid ('chr       ', 3, chrid);
      enterstandardid ('succ      ', 4, succid);
      enterstandardid ('pred      ', 4, predid);
      enterstandardid ('maxint    ', 6, maxintid);
      enterstandardid ('seek      ', 4, seekid);
      enterstandardid ('page      ', 4, pageid);
      enterstandardid ('time      ', 4, timeid);
      enterstandardid ('size      ', 4, sizeid);
      enterstandardid ('bitsize   ', 7, bitsizeid);
      enterstandardid ('upper     ', 5, upperid);
      enterstandardid ('lower     ', 5, lowerid);

      enterstandardid ('loophole  ', 8, loopholeid);
      enterstandardid ('ref       ', 3, refid);

      enterstandardid ('noioerror ', 9, noioerrorid);
      enterstandardid ('ioerror   ', 7, ioerrorid);
      enterstandardid ('iostatus  ', 8, iostatusid);
      enterstandardid ('delete    ', 6, deleteid);
      enterstandardid ('rename    ', 6, renameid);

      enterstandardid ('forward   ', 7, forwardid);
      enterstandardid ('external  ', 8, externalid);
      enterstandardid ('nonpascal ', 9, nonpascalid);
      enterstandardid ('interrupt ', 9, interruptid);

      enterstandardid ('minint    ', 6, minintid);
      enterstandardid ('shortint  ', 8, shortintid);
      enterstandardid ('insert    ', 6, insertid);
      enterstandardid ('str       ', 3, strid);
      enterstandardid ('val       ', 3, valprocid);
      enterstandardid ('copy      ', 4, copyid);
      enterstandardid ('concat    ', 6, concatid);
      enterstandardid ('length    ', 6, lengthid);
      enterstandardid ('pos       ', 3, posid);
      enterstandardid ('deletestr ', 9, deletestrid);

      enterstandardid ('facos     ', 5, facosid);
      enterstandardid ('fasin     ', 5, fasinid);
      enterstandardid ('fatan     ', 5, fatanid);
      enterstandardid ('fatanh    ', 6, fatanhid);
      enterstandardid ('fcosh     ', 5, fcoshid);
      enterstandardid ('fetoxm1   ', 7, fetoxm1id);
      enterstandardid ('fgetexp   ', 7, fgetexpid);
      enterstandardid ('fgetman   ', 7, fgetmanid);
      enterstandardid ('fint      ', 4, fintid);
      enterstandardid ('flog10    ', 6, flog10id);
      enterstandardid ('flog2     ', 5, flog2id);
      enterstandardid ('flognp1   ', 7, flognp1id);
      enterstandardid ('fmod      ', 4, fmodid);
      enterstandardid ('frem      ', 4, fremid);
      enterstandardid ('fscale    ', 6, fscaleid);
      enterstandardid ('fsgldiv   ', 7, fsgldivid);
      enterstandardid ('fsglmul   ', 7, fsglmulid);
      enterstandardid ('fsinh     ', 5, fsinhid);
      enterstandardid ('ftan      ', 4, ftanid);
      enterstandardid ('ftanh     ', 5, ftanhid);
      enterstandardid ('ftentox   ', 7, ftentoxid);
      enterstandardid ('ftwotox   ', 7, ftwotoxid);
      enterstandardid ('fsincos   ', 7, fsincosid);
      enterstandardid ('fmovecr   ', 7, fmovecrid);
      enterstandardid ('setfpcr   ', 7, setfpcrid);
      enterstandardid ('readfpcr  ', 8, readfpcrid);
    end;
    {>>>}

  begin
    sharedPtr := getSharedPtr;
    tokenSharedPtr := gettokenSharedPtr;

    { End of special configuration checks}
    sharedPtr^.curstringbuf := true;
    nextswitchread := false;

    initscanswitches;
    inittokentable;

    for i := 0 to hashtablesize do
      begin
      hashtable[i].pos := 0;
      hashtable[i].len := 0;
      end;

    resindex := 0;
    initreswords;
    initstandardids;

    incomment := false;
    inliteralstring := false;
    charcount := 0;
    sharedPtr^.lastline := 1;

    lasttokenline := 1;
    baseline[1] := 0;
    lastbaseline := - 1;
    tokenbufindex := 0;
    convertingcase := true;
    endofinput := false;

    linepos := inputbufsize;
    skippingblanks := false;

    { Initialize some switches }
    first_real_seen := false; {used to detect error when $double occurs after first real constant.}
    first_token_seen := false; {used to detect error when $case occurs after first token is scanned.}

    { init the upper to lower case conversion table }
    mapchars['A'] := 'a';
    mapchars['B'] := 'b';
    mapchars['C'] := 'c';
    mapchars['D'] := 'd';
    mapchars['E'] := 'e';
    mapchars['F'] := 'f';
    mapchars['G'] := 'g';
    mapchars['H'] := 'h';
    mapchars['I'] := 'i';
    mapchars['J'] := 'j';
    mapchars['K'] := 'k';
    mapchars['L'] := 'l';
    mapchars['M'] := 'm';
    mapchars['N'] := 'n';
    mapchars['O'] := 'o';
    mapchars['P'] := 'p';
    mapchars['Q'] := 'q';
    mapchars['R'] := 'r';
    mapchars['S'] := 's';
    mapchars['T'] := 't';
    mapchars['U'] := 'u';
    mapchars['V'] := 'v';
    mapchars['W'] := 'w';
    mapchars['X'] := 'x';
    mapchars['Y'] := 'y';
    mapchars['Z'] := 'z';
  end;
  {>>>}

begin
  initscanner;

  sharedPtr^.sourcelevel := 1;
  sharedPtr^.curfile := 1;
  openNext;

  logInputFilename;

  nextch := ' ';
  getch;
end;
{>>>}
{<<<}
procedure scan2;

begin
  seekstringfile (sharedPtr^.stringfilecount);

  dumpidentifiers;

  close (sharedPtr^.source[sharedPtr^.sourcelevel]);
  sharedPtr^.sourcelevel := 0;

  with tokenSharedPtr^.nexttoken do
    begin
    token := eofsym;
    line := sharedPtr^.lastline;
    left := chpos;
    right := chpos
    end;

  puttoken;
  if tokenbufindex > 0 then
    put (tokenSharedPtr^.tokenFile);

  sharedPtr^.stringtablelimit := sharedPtr^.stringfilecount + sharedPtr^.stringtabletop;
  dispose (sharedPtr^.stringtable);

  Writeln ('scan found ', tokenSharedPtr^.tokenCount:3, ' tokens');
end;
{>>>}
{<<<}
procedure scan;
{ Main scan procedure, see forward declaration for more data }

begin
  scan1;

  repeat
    scantoken;
    until (sharedPtr^.fatalflag or endofinput);

  { align strings }
  while sharedPtr^.stringfilecount mod stringroundoff <> 0 do
    begin
    sharedPtr^.stringblkptr^[sharedPtr^.nextstringfile] := 0;
    putstringfile;
    sharedPtr^.stringfilecount := sharedPtr^.stringfilecount + 1;
    end;

  scan2;
end;
{>>>}
