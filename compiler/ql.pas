{ ql.pas }
PROGRAM ql (input,output);
{<<<}
const
  maxStringLen = 132;

  maxHash = 4095;

  esc = 27;
  startbase = %x'400';

  { probably chosen to match ethernet download packet size }
  bytesPerFileRec$ = 1536;
  recsPerFileRec$ = {102} (bytesPerFileRec$ - 4) DIV 15;
{>>>}
{<<<}
type
  byte = 0..255;
  word = -32768..32767;

  filenameType = varying [80] of char;
  string = varying [maxStringLen] of char;
  objrec = varying [255] of char;

  symbolNameType = packed array [1..10] of char;
  {<<<}
  idrec = packed record
    rlen    : word;
    rtype   : char;
    modName : symbolNameType;
    end;
  {>>>}

  referencePtr = ^reference;
  {<<<}
  reference = packed record
    next : referencePtr;
    symbolName : symbolNameType;
    end;
  {>>>}

  resolvePtr = ^resolve;
  {<<<}
  resolve = packed record
    next : resolvePtr;
    addr   : integer;
    offset : integer;
    end;
  {>>>}

  symbolPtr = ^symbolType;
  {<<<}
  symbolType = packed record
    nextSymbol : symbolPtr;
    nextCommon : symbolPtr;
    symbolName : symbolNameType;
    modName    : symbolNameType;
    section    : integer;
    addr       : integer;
    comsize    : integer;
    def        : boolean;
    used       : boolean;
    flagged    : boolean;
    hist       : boolean;
    refList    : referencePtr;
    resList    : resolvePtr;
    end;
  {>>>}

  block = packed array [0..255] of byte;
  bblock = packed array [0..511] of byte;

  fileListPtrType = ^fileListType;
  {<<<}
  fileListType = record
    next : fileListPtrType;
    f : text;
    end;
  {>>>}

  {<<<}
  milestone = record
    millTime  : integer;
    intTime   : integer;
    timeOfDay : packed array [1..11] of char;
    end;
  {>>>}

  { history }
  historyType = ($historyObj, $historySymbol, $historyRef);
  {<<<}
  historyRecordType = record
    CASE historyType : historyType of
      $historyObj    : (obj_addr : integer;);
      $historySymbol : (symbol_addr : integer;
                        symbol_name : symbolNameType;);
      $historyRef    : (ref_addr: integer;
                        ref_offset : integer;);
    end;
  {>>>}

  {<<<}
  filedHistoryRecordType = record CASE boolean of
    true : (numRecs : integer;
            recs    : ARRAY [1 .. recsPerFileRec$] of historyRecordType;);
    false: (dummy   : packed array [1 .. bytesPerFileRec$] of char);
    end;
  {>>>}
  historyFileType = FILE of filedHistoryRecordType;
{>>>}
{<<<}
var
  command: string;
  commandLen: word;

  fileId: string;
  filename: filenameType;
  curFile: filenameType;
  ext: filenameType;
  defExt: filenameType;
  commandRoot: filenameType;
  fullFilename: filenameType;

  cmdFile: fileListPtrType;
  logFile: text;
  objectFile : FILE of block;
  textObjectFile: text;
  moduleFile: text;
  targetFile: text;
  srFormatFile: text;
  binaryFile: FILE of bblock;
  downloadTargetFile: FILE of bblock;

  termchar: char;
  smax, checksum, pass, opos, bpos: integer;

  blockPtr: integer;
  o: objrec;

  { switches }
  modules, download, check, bell, xref, map, bin, out, symout : boolean;
  chat, debug, logging, english, quiet, files, history, escape : boolean;

  duffer: boolean;
  newline: boolean;
  inpacket: boolean;
  usingHistory: boolean;

  prevCommon: symbolPtr;
  commonHead: symbolPtr;

  numSymbols: integer;
  numUndefinedSymbols: integer;
  hashTable: array [0..maxHash] of symbolPtr;

  codestart, codelen: integer;
  topESD: integer;
  userbase, sbase, sectbase, baseaddr: array [-1..15] of integer; {element -1 is the ABS section}

  esdArray: array [0..255] of integer;
  esdSymbolArray: array [0..255] of symbolPtr;
  outAddrArray: array [0..255] of integer;
  codeArray: array [1..64] of integer;

  startLink, endPass1, endPass2, endLink: milestone;
  endMapGen, endHisGen, endSymGen, endSpaceAlloc, endXrefGen: milestone;
  startReadHis, endReadHis: milestone;

  modName: symbolNameType;

  oblock: bblock;
  bl: block;
  inblock: block;
  objectEof: boolean;

  sn: symbolNameType;
  spt: symbolPtr;
  orec: objrec;
  total, basepos, i: integer;

  datestring: packed array [1..11] of CHAR;
{>>>}

  {<<<  utils}
  {<<<}
  function seq (f1, f2: filenameType): boolean;

  begin
    if f1.length <> f2.length then
      seq := false
    else
      seq := f1 = f2;
  end;
  {>>>}

  {<<<}
  function ior (i1, i2: integer): integer;

  begin
    ior := ord (uor (uint(i1), uint(i2)));
  end;
  {>>>}
  {<<<}
  function iand (i1, i2: integer): integer;

  begin
    iand := ord(uand(uint(i1),uint(i2)));
  end;
  {>>>}
  {<<<}
  function ixor (i1, i2: integer): integer;

  begin
    ixor := ord(uxor(uint(i1),uint(i2)));
  end;
  {>>>}

  {<<<}
  function mvl (i: integer): integer;

  begin
    mvl := i*256;
  end;
  {>>>}
  {<<<}
  function mvr (i: integer): integer;

  begin
    if i < 0 then
      mvr := int (uor (%x'800000', uint (mvr (int (uand (%x'7FFFFFFF', uint(i)))))))
    else
      mvr := (i DIV 256) MOD (%x'1000000');
  end;
  {>>>}

  {<<<}
  function null (c: char): boolean;

  begin
    null := (c=chr(13)) OR (c=chr(10)) OR (c=' ') OR (c=chr(9)) OR (c=chr(0));
  end;
  {>>>}
  {<<<}
  function digit (c: char): boolean;

  begin
    digit := (c >= '0') AND (c <= '9');
  end;
  {>>>}

  {<<<}
  procedure forceUpperSymbol (var s: symbolNameType);

  var
    i:integer;

  begin
    for i := 1 TO 10 DO
      if (s[i] >= 'a') AND (s[i] <= 'z') then
        s[i] := chr (ord (s[i]) + ord('A') - ord('a'));
  end;
  {>>>}
  {<<<}
  procedure showModName;

  begin
    writeln ('in module ''', modName, ''', from file ''', curFile, '''....');
    if logging then
      writeln (logFile, 'in module ''', modName, ''', from file ''', curFile, '''....');
  end;
  {>>>}

  {<<<}
  function currentTime: milestone;

  var
    ms: milestone;
    temp: integer;

    {<<<}
    function getNum (startch: integer): integer;

    var
      temp1, temp2: integer;

    begin
      temp1 := ORD(ms.timeOfDay[startch]) - ORD('0');
      temp2 := ORD(ms.timeOfDay[startch + 1] ) - ORD('0');
      getNum := (temp1 * 10) + temp2;
    end;
    {>>>}

  begin
    time (ms.timeOfDay);

    { ms.timeOfDay is hh:mm:ss.cc }
    {                   12 45 78 AB }
    ms.intTime := 0;

    temp := getNum (1); {hh}
    ms.intTime := temp;

    temp := getNum (4); {mm}
    ms.intTime := ms.intTime * 60 + temp;

    temp := getNum (7); {ss}
    ms.intTime := ms.intTime * 60 + temp;

    temp := getNum (10); {cc}
    ms.intTime := ms.intTime * 100 + temp;

    currentTime := ms;
  end;
  {>>>}

  { milestone }
  {<<<}
  procedure clearMilestone (var ms: milestone);

  begin
    ms.millTime := 0;
    ms.intTime := 0;
    ms.timeOfDay := '           ';
  end;
  {>>>}
  {<<<}
  procedure showMilestone (s: string; ms1, ms2: milestone);

  var
    temp, cc, ss, mm, hh: integer;
    timeString: string;

  begin
    temp := ms1.intTime - ms2.intTime;

    cc := temp MOD 100;
    temp := temp DIV 100;
    ss := temp MOD 60;

    temp := temp DIV 60;
    mm := temp MOD 60;

    temp := temp DIV 60;
    hh := temp MOD 60;

    write (s);
    write (ms1.timeOfDay,' ',(ms1.millTime-ms2.millTime) / 1000:7:2);
    writev (timeString, hh :2, ':', mm :2, ':', ss :2, '.', cc :2 );

    for temp := 1 TO timeString.length DO
      if timeString[temp] = ' ' then
        timeString[temp] := '0';

    write ( ' ', timeString );

    if endLink.millTime - startLink.millTime > 0 then
      write ('  ', ((ms1.millTime-ms2.millTime)*100) / (endLink.millTime-startLink.millTime):7:2,'%');
    writeln;
  end;
  {>>>}
  {>>>}
  {<<<  string utils}
  {<<<}
  { string variables of  type "packed array [0..n] of char", where n >= 1.
    The length of the string is stored in element 0, elements 1 to n are the characters of the string.

    The routines will also accept parameters of type "packed array [1..n] of char", n length of string.
    Quoted strings in Pascal are considered to be of this type,
      may be mixed with string variables with calling routines that don't require "var" params.

    The start and span parameters define a substring beginning at position start
      (between characters start-1 and start) with a length of abs(span).

    If span is positive, the substring is to the right of Start
            if negative, the substring is to the left.
  }
  {>>>}
  {<<<}
  procedure assign (var t: packed array [tlow..thigh: integer] of char;
                   s: packed array [slow..shigh: integer] of char);
  { Assign (T,S) - Assign string S to the target string T }
  {   useful for assigning a literal string to a variable string }

  var
    slen: integer;
    i: integer;

  begin
    if (tlow <> 0) and (tlow <> 1) or (slow <> 0) and (slow <> 1) then
      writeln  ('assign - bad arguments error');
    if slow = 0 then
      slen := ord(s[0])

     {User option here - the following code removes all trailing blanks
      from a literal string when inserting it into a variable string...
      This may be annoying to some users as you may wish to actually assign
      blanks to the end of a string in this manner.  To change the code
      comment out the following :

          else begin
           slen := shigh;
           while (s.....
           end;
      and replace the "else begin" with
         else slen := shigh;
     }
    else
      begin
      slen := shigh;
      while (s[slen] = ' ') and (slen > 1) do
        slen := slen - 1;
      end;

    if slen > thigh then
      writeln ('assign - destination string too short error');

    for i := 1 to slen
      do t[i] := s[i];

    if tlow = 0 then
      t[0] := chr(slen)
    else
      if tlow = 1 then
        for i := slen + 1 to thigh do t[i] := ' ';
  end;
  {>>>}
  {<<<}
  procedure assignChar (var t: packed array [tlow..thigh: integer] of char; c: char);
  { AssChar (T,C) - Assign character C to the target string T }
  {   useful for assigning a single character to a variable string}

  begin
    if tlow <> 0 then
      writeln ('assignchar error');

    t[0] := chr(1);
    t[1] := c;
  end;
  {>>>}
  {<<<}
  procedure clear (var s: packed array [low..high: integer] of char);
  { clear (S) - initializes string S to objectEof }

  begin
    if low <> 0 then
      writeln ('clear error')
    else
      s[0] := chr(0);
  end;
  {>>>}

  {<<<}
  function len (s: packed array [slow..shigh: integer] of char): integer;
  { Len (S) - a function giving the current length of string S }

  begin
    if slow = 0 then
      len := ord (s[0])
    else if slow = 1 then
      len := shigh
    else
      writeln ('len');
  end;
  {>>>}
  {<<<}
  function equal (s1: packed array [s1low..s1high: integer] of char;
                  s2: packed array [s2low..s2high: integer] of char): boolean;
  { Equal (T,S) - Function Equal returns TRUE when T=S, returns FALSE otherwise }

  var
    s1len, s2len: integer;
    eq: boolean;
    i: integer;

  begin
    if (s1low <> 0) and (s1low <> 1) or
       (s2low <> 0) and (s2low <> 1) then
      writeln ('equal - bad arguments error')

    else
      begin
      if (s1low = 0) then
        s1len := ord(s1[0])
      else
        begin
        s1len := s1high;
        while (s1[s1len] = ' ') and (s1len > 1) do
          s1len := s1len - 1;
        end;

      if (s2low = 0) then
        s2len := ord(s2[0])
      else
        begin
        s2len := s2high;
        while (s2[s2len] = ' ') and (s2len > 1) do
          s2len := s2len - 1;
        end;
      if s1len <> s2len then
        equal := false
      else
        begin
        eq := true;
        for i := 1 to s1len do eq := eq and (s1[i] = s2[i]);
        equal := eq;
        end;
      end;
  end;
  {>>>}

  {<<<}
  function search (s: packed array [slow..shigh: integer] of char;
                  t: packed array [tlow..thigh: integer] of char;
                  start: integer): integer;
  { search (S,T,Start) - searches string S for the first  }
  {   occurrence of string T to the right of position Start (characters are numbered beginning with one)}
  {   returns the position of the first character in the matching substring }
  {     or the value zero if the string T does not appear }

  var
    i, j, tlen, slen: integer;
    uneq: boolean;

  begin
    if (start < 1) or (slow <> 0) and (slow <> 1) or (tlow <> 0) and (tlow <> 1) then
      writeln ('search error')

    else
      begin
      search := 0;

      if slow = 0 then
        slen := ord(s[0])
      else
        slen := shigh;

      if tlow = 0 then
        tlen := ord(t[0])
      else
        tlen := thigh;

      if (start + tlen <= slen + 1) and (tlen <> 0) then
        begin
        i := start - 1;
        repeat
          i := i + 1;
          j := 0;
          repeat
            j := j + 1;
            uneq := t[j] <> s[i + j - 1];
          until uneq or (j = tlen);
        until (not uneq) or (i = slen - tlen + 1);

        if uneq then
          search := 0
        else
          search := i;
        end;
      end;
  end;
  {>>>}

  {<<<}
  procedure insert (var t: packed array [tlow..thigh: integer] of char;
                   s: packed array [slow..shigh: integer] of char;
                   p: integer);
  { insert (T,S,Start) - inserts the string S into the target string T at position Start }
  {   Characters are shifted to the right as necessary }
  {   Overflow produces a truncated target string }
  {   a Start position which would produce a string which was not contiguous has no effect }

  var
    i, j, tlen, slen: integer;

  begin
    if (tlow <> 0) or (slow <> 0) and (slow <> 1) then
      writeln ('insert error1')

    else
      begin
      tlen := ord(t[0]);
      if slow = 0 then
        slen := ord(s[0])
      else
        slen := shigh;
      if slen > 0 then
        if (p > 0) and (p <= tlen + 1) then
          begin
          if slen + tlen >= thigh then
            tlen := thigh
          else
            tlen := slen + tlen;
          for i := tlen downto p + slen do
            t[i] := t[i - slen];

          if tlen < p + slen then
            j := tlen
          else
            j := p + slen - 1;

          for i := p to j do t[i] := s[i - p + 1];
          t[0] := chr(tlen);
          end
        else
          writeln  ('insert error2') { error: non-contiguous string } ;
      end;
  end;
  {>>>}
  {<<<}
  procedure concatenate (var t: packed array [tlow..thigh: integer] of char;
                         s: packed array [slow..shigh: integer] of char);
  { concatenate (T,S) - appends string S to the target string T. The resulting value is string T }
  {   Overflow results in truncation to StringMax characters }

  var
    i, slen, tlen: integer;

  begin
    if (tlow <> 0) or (slow <> 1) and (slow <> 0) then
      writeln ('concatenate error')

    else
      begin
      if slow = 1 then
        slen := shigh
      else
        slen := ord(s[0]);

      tlen := ord(t[0]);
      if slen + tlen > thigh then
        slen := thigh - tlen;

      t[0] := chr(slen + tlen);
      for i := 1 to slen do
        t[i + tlen] := s[i];
      end;
  end;
  {>>>}
  {<<<}
  procedure subString (var t: packed array [tlow..thigh: integer] of char;
                       s: packed array [slow..shigh: integer] of char;
                       start, span: integer);
  { Substring (T,S,Start,Span) - the substring of string S defined by Start, Span is assigned to the target string T }

  var
    i, slen: integer;

  begin
    if (tlow <> 0) or (slow <> 0) and (slow <> 1) then
      writeln ('substring error1')

    else
      begin
      if slow = 0 then
        slen := ord(s[0])
      else
        slen := shigh;

      if span < 0 then
        begin
        span := - span;
        start := start - span
        end;

      if start < 1 then
        begin
        span := span + start - 1;
        start := 1
        end;

      if start + span > slen + 1 then
        span := slen - start + 1;
      if thigh < span then
        writeln ('substring error2')
      else if span <= 0 then
        t[0] := chr(0)
      else
        begin
        for i := 1 to span do t[i] := s[start + i - 1];
        t[0] := chr(span);
        end;
      end;
  end;
  {>>>}
  {<<<}
  procedure deleteString (var t: packed array [tlow..thigh: integer] of char; start, span: integer);
  { deleteString (S,Start,Span)   - deletes the substring defined by Start, Span from the string S }

  var
    i, limit, tlen: integer;

  begin
    if tlow <> 0 then
      writeln ('deleteString error')

    else
      begin
      tlen := ord(t[0]);
      if span < 0 then
        begin
        span := - span;
        start := start - span
        end;

      limit := start + span;
      if start < 1 then
        start := 1;

      if limit > tlen + 1 then
        limit := tlen + 1;

      span := limit - start;
      if span > 0 then
        begin
        for i := 0 to tlen - limit do
          t[start + i] := t[limit + i];
        t[0] := chr(ord(t[0]) - span);
        end;
      end;
  end;
  {>>>}

  {<<<}
  procedure leftString (var t: packed array [tlow..thigh: integer] of char;
                        s: packed array [slow..shigh: integer] of char;
                        last: integer);
  { Leftstring (T,S,Last)      - the substring of string S to the left of Last is assigned to target string T }

  var
    i: integer;

  begin
    if (last >= shigh) or (last < 2) then
      writeln ('leftstring error')

    else
      begin
      for i := 1 to last do
        t[i] := s[i];

      t[0] := chr(last);
      end;
  end;
  {>>>}
  {<<<}
  procedure rightString (var t: packed array [tlow..thigh: integer] of char;
                         s: packed array [slow..shigh: integer] of char;
                         first: integer);
  { Rightstring (T,S,First)    - the substring of string S to the right of First is assigned to target string T }

  var
    i: integer;

  begin
    if (first < 1) or (first >= shigh) then
      writeln ('rightstring error')

    else
      begin
      for i := first to shigh do
        t[(i + 1) - first] := s[i];

      t[0] := chr(shigh - first + 1);
      end;
  end;
  {>>>}

  { misc char utils }
  {<<<}
  function isAlpha (c: char): boolean;

  begin
    isAlpha := ((c>='a') AND (c<='z')) OR ((c>='A') AND (c<='Z'));
  end;
  {>>>}
  {<<<}
  function isAlphaNum (c: char): boolean;

  begin
    isAlphaNum := digit(c) OR isAlpha(c) OR (c='_');
  end;
  {>>>}
  {<<<}
  function chToHex (c: char): integer;

  begin
    if (c >= '0') AND (c <= '9') then
      chToHex := ord(c)-ord('0')
    else if (c >= 'a') AND (c <= 'f') then
      chToHex := ord(c)-ord('a')+10
    else if (c >= 'A') AND (c <= 'F') then
      chToHex := ord(c)-ord('A')+10
    else
      begin
      writeln ('Duff char ''', c,'''when hex char expected!');
      chToHex := 0;
      end;
  end;
  {>>>}
  {>>>}
  {<<<  file utils}
  {<<<}
  function getExt (var filename: filenameType; defaultExt: filenameType): filenameType;

  var
    i:integer;
    result: filenameType;

  begin
    i := filename.length;

    { search backwards, to see if first non-alpha char is a '.' (i.e. extension)}
    while (i > 1) AND isAlphaNum (filename[i]) DO
      i := i - 1;

    if filename[i] = '.' then
      begin
      subString (result, filename, i, filename.length + 1 - i);
      getExt := filename;
      end
    else
      begin
      filename := filename + defaultExt;
      getExt := defaultExt;
      end;
  end;
  {>>>}

  { logging file }
  {<<<}
  procedure openloggingFile;

  begin
    rewrite (logFile, commandRoot + '.log');
    writeln (logFile, 'Linking from ', fullFilename);
  end;
  {>>>}
  {<<<}
  procedure closeloggingFile;

  var
    total, i:integer;
    datestring: packed array [1..11] of CHAR;

  begin
    if english then
      begin
      writeln (logFile);

      total := 0;
      if sectbase[8] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of P                 (8)  = ', sectbase[8]:8, ' bytes');
        total := total + sectbase[8];
        end;
        {>>>}
      if sectbase[9] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of HELP              (9)  = ', sectbase[9]:8, ' bytes');
        total := total + sectbase[9];
        end;
        {>>>}
      if sectbase[12] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of error messages   (12)  = ', sectbase[12]:8, ' bytes');
        total := total + sectbase[12];
        end;
        {>>>}
      if sectbase[13] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of code & constants (13)  = ', sectbase[13]:8, ' bytes');
        total := total + sectbase[13];
        end;
        {>>>}
      if sectbase[14] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of diagnostic block (14)  = ', sectbase[14]:8, ' bytes');
        total := total + sectbase[14];
        end;
        {>>>}
      if sectbase[15] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of global variables (15)  = ', sectbase[15]:8, ' bytes');
        total := total + sectbase[15];
        end;
        {>>>}

      writeln (logFile, 'Total size                     = ', total:8, ' bytes');
      end

    else for i:=0 TO 15 DO
      if sectbase[i] <> 0 then
        begin
        write (logFile, 'Section ',i:2,' Start ', hex(baseaddr[i], 6, 6), ' Length ', hex(sectbase[i],6,6));
        writeln (logFile, ' Finish  ', hex(baseaddr[i] + sectbase[i], 6 ,6));
        end;

    date (datestring);
    writeln (logFile);
    writeln (logFile, 'Link started ', startLink.timeOfDay, ' ', datestring);
    writeln (logFile, 'Link ended   ', endLink.timeOfDay, ' ', datestring);
    writeln (logFile, 'total CPU time:- ',(endLink.millTime-startLink.millTime)/1000:7:2);

    close (logFile);
  end;
  {>>>}

  { .ro objext file }
  {<<<}
  procedure openin (filename: filenameType);

  begin
    curFile := filename;
    reset (objectFile, filename);

    blockPtr := 0;
    read (objectFile, inblock);

    objectEof := false;
  end;
  {>>>}
  {<<<}
  procedure getRecord (var o: objrec);
  { .ro files are 256 byte fixed size blocks. Within these blocks, records are
    packed end to end i.e. one record can span a block boundary. Each record
    conisits of a single byte <n> followed by <n> data bytes
  }
  var
    i, l1:integer;

  begin
    o.length := inblock[blockPtr];

    if (255-blockPtr) > o.length then
      l1 := o.length
    else
      l1 := (255-blockPtr);

    for i := 1 TO l1 DO
      o[i] := chr(inblock[blockPtr+i]);

    blockPtr := blockPtr+l1+1;
    if (blockPtr > 255) then
      begin
      if eof (objectFile) then
        objectEof := true
      else
        read (objectFile,inblock);

      blockPtr := 0;
      l1 := l1 + 1; {step to start of next xfer}
      for i := l1 TO o.length DO
        o[i] := chr (inblock[i - l1]);
      blockPtr := 1 + o.length - l1;
      end;
  end;
  {>>>}
  {<<<}
  procedure closeIn;

  begin
    close (objectFile);
  end;
  {>>>}

  { .rx objext file }
  {<<<}
  procedure openTextIn (filename: filenameType);

  begin
    curFile := filename;
    reset (textObjectFile, filename);
  end;
  {>>>}
  {<<<}
  procedure getTextRec (var o: objrec);
  { .rx files are a text version of the .ro file. Each record is a single line
    of text, written out as hex characters i.e. 2 characters per byte. The record
    length is derived from the bytes on the line. This format is provided to allow
    .rx files to be easily ported from other systems e.g. Unix
  }
  var
    bytes, i: integer;
    buff: varying [255] of char;

  begin
    readln (textObjectFile, buff);
    bytes := buff.length DIV 2;
    o.length := bytes;
    for i := 1 TO bytes DO
      o[i] := chr (chToHex(buff[i*2-1]) * 16 + chToHex(buff[i*2]));
  end;
  {>>>}
  {<<<}
  procedure closeTextIn;

  begin
    close (textObjectFile);
  end;
  {>>>}

  { binary output file }
  {<<<}
  procedure binByte (b: byte);

  begin
    oblock[opos] := b;
    opos := opos + 1;

    if opos > 511 then
      begin
      if out then
        write (binaryFile, oblock);
      if download then
        write (downloadTargetFile, oblock);
      opos := 0;
      end;

  end;
  {>>>}
  {<<<}
  procedure sendBin (b: byte);

  begin
    if (b = esc) AND (escape = true) then
      binbyte (b);

    binbyte (b);
  end;
  {>>>}
  {<<<}
  procedure sendsfnewline;

  begin
    if download then
      writeln (targetFile);
    if out then
      writeln (srFormatFile);
  end;
  {>>>}
  {<<<}
  procedure sendsform (var s: string);

  begin
    if out then
      write (srFormatFile,s);
    if download then
      write (targetFile,s);
  end;
  {>>>}
  {<<<}
  procedure wbyte (b: byte);

  var
    s: string;

  begin
    if bin then
      begin
      sendbin (b);
      checksum := ixor (checksum,b);
      end
    else
      begin
      s := hex (b,2,2);
      sendsform (s);
      checksum := checksum + b;
      end;
  end;
  {>>>}
  {<<<}
  procedure endPacket;

  var
    s:string;

  begin
    if bin then
      sendbin (checksum)
    else
      begin
      s := hex (255 - (checksum MOD 256),2,2);
      sendsform (s);
      sendsfnewline;
      end;
  end;
  {>>>}
  {<<<}
  procedure sendStop;

  var
    endstr : string;
    I : integer;

  begin
    if bin then
      BEGIN
      checksum := 0;
      binbyte (esc);
      binbyte (0);
      wbyte (2);
      wbyte (0);
      wbyte (4);

      for i := 1 TO 4 DO
        wbyte(0);
      endPacket;

      for i := 0 TO 511 DO
        binbyte (0);
      end

    else
      begin
      endstr := 'S9030000FC';
      sendsform (endstr);
      sendsfnewline;
      end;
  end;
  {>>>}

  { dump to file }
  {<<<}
  procedure dumpSymbols;

  var
    i:integer;
    bl: block;
    symbol: symbolPtr;
    symbolTableFile: file of block;

    {<<<}
    procedure pbyte (b: byte);

    begin
      bl[bpos] := b;
      bpos := bpos + 1;

      if bpos > 255 then
        begin
        write (symbolTableFile, bl);
        bpos := 0;
        end;
    end;
    {>>>}
    {<<<}
    procedure outdata (s: string);

    var
      i:integer;

    begin
      pbyte (s.length);
      for i := 1 TO s.length DO
        pbyte (ord(s[i]));
    end;
    {>>>}
    {<<<}
    function binInt (val: integer): string;

    var
      b: string;
      i: integer;

    begin
      b.length := 4;
      for i := 4 downto 1 DO
        begin
        b[i] := chr (val);
        val := mvr (val);
        end;

      binInt := b;
    end;
    {>>>}
    {<<<}
    procedure outSymbol (symbol: symbolPtr);

    begin
      WITH symbol^ DO
        begin
        outdata ('1' + modName + 'VRLvvvvuuccccccccffffffffxxtttddd');
        outdata ('2' + chr(%x'50') + symbolName + binInt (addr + baseaddr[section]));
        end;
    end;
    {>>>}

  begin
    bpos := 0;

    rewrite (symbolTableFile, commandRoot + '.sym');

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        symbol := hashTable[i];
        repeat
          outSymbol (symbol);
          symbol := symbol^.nextSymbol;
          until symbol = nil;
        end;

      outdata ('4' + chr(17) + binInt(0)); { module end record }
      if bpos > 0 then
        begin
        for i := bpos TO 255 DO
          bl[i] := 0;
        write (symbolTableFile, bl);
        end;

    close (symbolTableFile);
  end;
  {>>>}
  {<<<}
  procedure dumpSymbolMap;

  var
    i:integer;
    s_ptr: symbolPtr;
    map_file : text;

  begin
    rewrite (map_file, commandRoot+'.map');

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        if debug then
          writeln (map_file, i, ':');
        s_ptr := hashTable[i];

        repeat
          write (map_file,s_ptr^.symbolName,' ',
          hex (s_ptr^.addr+baseaddr[s_ptr^.section],6,6),' ',s_ptr^.modName);
          if s_ptr^.comsize<>-1 then
            writeln (map_file,' C:', hex (s_ptr^.comsize,4,4))
          else
            if NOT s_ptr^.def then
              writeln (map_file,' Undef!')
            else
              writeln (map_file);
          s_ptr := s_ptr^.nextSymbol;
          until s_ptr = nil;
      end;

    close (map_file);
  end;
  {>>>}
  {<<<}
  procedure dumpXreferences;
  { dump cross references to file from list held per symbol }

  var
    i,refcount: integer;
    s_ptr: symbolPtr;
    ref_file: text;
    r: referencePtr;

  begin
    rewrite (ref_file, commandRoot + '.xrf');

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        if debug then
          writeln (ref_file,i,':');
        s_ptr := hashTable[i];
        REPEAT
          refcount:=0;
          write (ref_file,s_ptr^.symbolName,' ',
                 hex (s_ptr^.addr + baseaddr[s_ptr^.section], 6, 6),' ',s_ptr^.modName);
          if s_ptr^.comsize<>-1 then
            writeln (ref_file,' C:',hex(s_ptr^.comsize,4,4))
          else
            if NOT s_ptr^.def then
              writeln (ref_file,' Undef!')
            else
              writeln (ref_file);

          if s_ptr^.refList<>nil then
            begin
            r := s_ptr^.refList;
            REPEAT
              write (ref_file,r^.symbolName,'    ');
              refcount := refcount + 1;
              if refcount MOD 6 = 0 then
                writeln (ref_file);
              r := r^.next;
              until r = nil;
            if refcount MOD 6 <> 0 then
              writeln (ref_file);
          end
        else
          writeln(ref_file,'Not referenced ANYWHERE!');

        writeln
          (ref_file,'--------------------------------------------------------------------------');
        s_ptr:=s_ptr^.nextSymbol;
        until s_ptr=nil;
      end;

    close (ref_file);
  end;
  {>>>}
  {<<<}
  procedure dumpHistory;
  { disp history and readHistory must have match in file format }

  var
    i, rescount:integer;
    s_ptr: symbolPtr;
    r : resolvePtr;

    historyRecord : historyRecordType;
    filedHistoryRecord : filedHistoryRecordType;
    res_file : historyFileType;

    {<<<}
    procedure send_to_file (rec: historyRecordType);

    begin
      filedHistoryRecord.numRecs := filedHistoryRecord.numRecs + 1;
      filedHistoryRecord.recs[filedHistoryRecord.numRecs] := rec;
      if filedHistoryRecord.numRecs = recsPerFileRec$ then
        begin
        Write (res_file, filedHistoryRecord);
        filedHistoryRecord.numRecs := 0;
        end;
    end;
    {>>>}

  begin
    rewrite (res_file, commandRoot + '.his');
    filedHistoryRecord.numRecs := 0;

    historyRecord.historyType := $historyObj;
    historyRecord.obj_addr := basepos;

    { Write (res_file, historyRecord); }
    send_to_file (historyRecord);

    for i:=0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        s_ptr := hashTable[i];

        repeat
          begin
          if s_ptr^.comsize = -1 then { dont dump commons in history }
            begin
            historyRecord.historyType := $historySymbol;
            historyRecord.symbol_addr := s_ptr^.addr+baseaddr[s_ptr^.section];
            historyRecord.symbol_name := s_ptr^.symbolName;
            {historyRecord.mod_name := s_ptr^.modName;}
            {Write (res_file, historyRecord);}
            send_to_file (historyRecord);

            if s_ptr^.resList <> nil then
              begin
              r := s_ptr^.resList;
              repeat
                begin
                historyRecord.historyType := $historyRef;
                historyRecord.ref_addr := r^.addr;
                historyRecord.ref_offset := r^.offset;
                {Write (res_file, historyRecord);}
                send_to_file (historyRecord);
                r := r^.next;
                end until r = nil;
              end;

            end;

          s_ptr := s_ptr^.nextSymbol;
          end until s_ptr = nil;

        end;

    { Send the last one }
    if filedHistoryRecord.numRecs > 0 then
      Write (res_file, filedHistoryRecord);

    close (res_file);
  end;
  {>>>}
  {>>>}

  {<<<}
  function getByte: byte;

  begin
    getByte := ord (o[bpos]);
    bpos := bpos  +1;
  end;
  {>>>}
  {<<<}
  function getInt: integer;

  var
    i, j: integer;

  begin
    i := 0;
    for j := 1 TO 4 DO
      i := mvl (i) + getByte;

    getInt := i;
  end;
  {>>>}
  {<<<}
  function getSymbolName: symbolNameType;

  var
    i: integer;
    sn: symbolNameType;

  begin
    for i := 1 TO 10 DO
      sn[i] := chr(getByte);

    getSymbolName := sn;
  end;
  {>>>}

  {<<<}
  function symbolHash (var s: symbolNameType): integer;

  var
    hash, i: integer;

  begin
    hash := 0;

    i := 1;
    while (i < 10) AND (s[i] <> ' ') DO
      begin
      hash := hash * 97 + ord(s[i]);
      i := i + 1;
      end;

    symbolHash := hash MOD maxHash;
  end;
  {>>>}
  {<<<}
  function findInsert (var s: symbolNameType; var s_ptr: symbolPtr; ins: boolean): boolean;
  var
    found : boolean;
    hash : integer;

  begin
    forceUpperSymbol (s);
    hash := symbolHash (s);
    s_ptr := hashTable[hash];

    found := false;
    while (NOT found) AND (s_ptr <> nil) DO
      begin
      if s_ptr^.symbolName = s then
        found := true
      else
        s_ptr := s_ptr^.nextSymbol;
      end;

    findInsert := found;

    if (NOT found) AND ins then
      begin
      numSymbols := numSymbols + 1;
      new (s_ptr);
      s_ptr^.nextSymbol := hashTable[hash];
      hashTable[hash] := s_ptr;

      s_ptr^.def := false;
      s_ptr^.used := true;
      s_ptr^.flagged := false;
      s_ptr^.symbolName := s;
      s_ptr^.modName := modName;
      s_ptr^.comsize := -1;
      s_ptr^.refList := nil;
      s_ptr^.resList := nil;
      end;
  end;
  {>>>}
  {<<<}
  procedure allocCom;

  var
    s_ptr : symbolPtr;

  begin
    s_ptr := commonHead;
    while s_ptr <> nil DO
      begin
      s_ptr^.addr := sectbase[s_ptr^.section];
      sectbase[s_ptr^.section] := sectbase[s_ptr^.section]+s_ptr^.comsize;

      if odd(sectbase[s_ptr^.section]) then
        sectbase[s_ptr^.section] := sectbase[s_ptr^.section]+1;

      s_ptr := s_ptr^.nextCommon;
      end;
  end;
  {>>>}
  {<<<}
  procedure addRes (s: symbolPtr; addr, offset : integer);
  { add a resolved symbol reference to list held per symbol }

  var
    res_ptr: resolvePtr;

  begin
    new (res_ptr);
    res_ptr^.next := s^.resList;
    res_ptr^.addr := addr;
    res_ptr^.offset := offset;
    s^.resList := res_ptr;
  end;
  {>>>}
  {<<<}
  procedure overlapCheck;

  var
    i, j: integer;

    {<<<}
    function clash (i, j: integer):boolean;

    begin
      clash := NOT (((sectbase[i]+baseaddr[i])<=baseaddr[j]) OR
                    ((sectbase[j]+baseaddr[j])<=baseaddr[i]));
    end;
    {>>>}

  begin
    for i := 0 TO 14 DO
      for j := i+1 TO 15 DO
        if clash (i,j) then
          writeln ('Sections ',i:2,' and ',j:2,' overlap!');
  end;
  {>>>}
  {<<<}
  procedure checkUndefinedSymbols;

  var
    i:integer;
    s_ptr: symbolPtr;

  begin
    writeln ('Undefined symbols:-');
    if logging then
      writeln (logFile, 'Undefined symbols:-');

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        s_ptr := hashTable[i];

        repeat
          if NOT s_ptr^.def then
            begin
            writeln ('''',s_ptr^.symbolName,''' first referenced in module ''', s_ptr^.modName,'''');
            if logging then
              writeln (logFile, '''', s_ptr^.symbolName, ''' first referenced in module ''', s_ptr^.modName, '''');
            s_ptr^.section := -1;
            s_ptr^.addr := %X'FAFF';
            end;

          s_ptr := s_ptr^.nextSymbol;
          until s_ptr = nil;
        end;
  end;
  {>>>}
  {<<<}
  procedure doubleDef (s: symbolPtr);

  begin
    showModName;

    writeln ('Doubly defined label  ''',s^.symbolName,'''');
    writeln ('Previously defined in module ''',s^.modName,'''');

    if logging then
      begin
      writeln (logFile, 'Doubly defined label  ''',s^.symbolName,'''');
      writeln (logFile, 'Previously defined in module ''',s^.modName,'''');
      end;

    s^.flagged := true;
  end;
  {>>>}
  {<<<}
  procedure readHistory (filename: filenameType);
  { disp history and readHistory must have match in file format }

  var
    spt : symbolPtr;
    r : resolvePtr;

    filedHistory : filedHistoryRecordType;
    historyFile : historyFileType;

  begin
    spt := nil;

    reset (historyFile, filename);

    while NOT eof (historyFile) DO
      begin
      Read (historyFile, filedHistory);

      for i := 1 TO filedHistory.numRecs DO
        WITH filedHistory.recs[ i ] DO CASE historyType of
          $historyObj :
            basepos := obj_addr;

          $historySymbol :
            begin
            if findInsert (symbol_name, spt, true) then;
            spt^.hist := TRUE;
            spt^.modName := 'patched!!!';
            spt^.section := -1;
            spt^.def := true;
            spt^.addr := symbol_addr;
            end;

          $historyRef :
            begin
            new (r);
            r^.next := spt^.resList;
            r^.addr := ref_addr;
            r^.offset := ref_offset;
            spt^.resList := r;
            end;
          end;

      end;

    close (historyFile);
  end;
  {>>>}

  {<<<}
  procedure switchSettingsProcess (var s: string);

  var
    i, j, l: integer;
    slashfound: boolean;
    lineBuffer: packed array [1..maxStringLen+1] of char; {extra byte to stop bound overflow!}

    {<<<}
    procedure doSwitch (start, switchLen: integer);
    { processes a switch setting directive }

    var
      c : char;
      noflag : boolean;
      tla : packed array [1..3] of char;
      i: integer;
      pos, endPos, switchEndPos: integer;
      section : integer;

      {<<<}
      function getNextCh: char;

      begin
        getNextCh := lineBuffer [pos];
        if pos < switchLen then
          pos := pos + 1;
      end;
      {>>>}
      {<<<}
      procedure setSwitch (var b: boolean);

      BEGIN
        b := NOT noflag;
      end;
      {>>>}

    begin
      { convert to lowerCase }
      for i := 1 TO switchLen DO
        if (lineBuffer[i] >= 'A') AND (lineBuffer[i] <= 'Z') then
          lineBuffer[i] := chr(ord(lineBuffer[i]) + ord('a') - ord('A'));

      pos := start;
      repeat
        noflag := false;
        tla[1] := getNextCh;
        tla[2] := getNextCh;
        tla[3] := '.';
        if tla = 'no.' then
          {<<<  no.}
          begin
          noflag := true;
          tla[1] := getNextCh;
          tla[2] := getNextCh;
          end;
          {>>>}

        tla[3] := getNextCh;
        switchEndPos := pos;

        { skip to next switch }
        repeat
          c := getNextCh;
          until (c='/') OR (null (c)) OR (pos >= switchLen);

        if (tla[1] = 'o') AND (tla[2] >= '0') AND (tla[2] <= '9') then
          {<<<  section startAddress}
          begin
          if tla[3] = ':' then
            section := ord (tla[2]) - ord('0')
          else
            begin
            section := 10 * (ord(tla[2]) - ord('0')) + (ord(tla[3]) - ord('0'));
            switchEndPos := switchEndPos + 1;
            end;

          if pos <> switchLen then
            endPos := pos - 2
          else
            endPos := pos;

          if (section >= 0) AND (section <= 15) then
            begin
            userbase[section] := 0;
            for i := switchEndPos TO endPos DO
              userbase[section] := 16 * userbase[section] + chToHex (lineBuffer[i]);
            if debug then
              writeln (hex (userbase[section], 6, 6), ' ', section);
            end
          else
            writeln (' Illegal section number in switch ', tla);
          end
          {>>>}
        else if (tla = 'xre') OR (tla = 'xrf') then
          setSwitch (xref) { generate xref file }
        else if tla = 'map' then
          setSwitch (map)  { generate map file}
        else if tla = 'sym' then
          setSwitch (symout) { generate symbol file}
        else if tla = 'bin' then
          setSwitch (bin)  { binary output}
        else if tla = 'mod' then
          setSwitch (modules) { module list}
        else if tla = 'deb' then
          setSwitch (debug) { debug mode}
        else if (tla = 'dld') OR (tla = 'dow') then
          setSwitch (download) {download to target}
        else if tla = 'out' then
          setSwitch (out) { generate any output at all!}
        else if tla = 'cha' then
          setSwitch (chat) { generate loads of output }
        else if tla = 'qui' then
          setSwitch (quiet) { generate minimum output}
        else if tla = 'eng' then
          setSwitch (english) { say understandable things}
        else if tla = 'log' then
          {<<<  log stuff in .log file}
          begin
          setSwitch (logging);
          openloggingFile;
          end
          {>>>}
        else if tla = 'fil' then
          setSwitch (files) { generate put filenames in mod file }
        else if tla = 'his' then
          setSwitch (history) { generate history file }
        else if tla = 'bel' then
          {<<<  bells}
          begin
          { generate bells at end of link }
          setSwitch (bell);
          if noflag then
            writeln ('What do you want, a prize or something??');
          end
          {>>>}
        else if tla = 'che' then
          setSwitch (check) { check all possible grubbies}
        else if tla = 'esc' then
          setSwitch (escape) { replace all 1B's in code with 1B1B }
        else
          writeln ('Unknown switch :', tla);
      until (pos >= switchLen) OR (null (c));
    end;
    {>>>}

  begin
    l := s.length;
    i := 1;
    slashfound := false;

    while (i < l) AND NOT slashfound DO
      if s[i] = '/' then
        slashfound := true
      else
        i := i + 1;

    if slashfound then
      begin
      for j := i + 1 TO l DO
        lineBuffer[j-i] := s[j]; { pick up everything after the slash }

      doSwitch (1, l-i);
      if i = 1 then
        s := ''
      else
        subString (s, s, 1, i-1);
      end;
  end;
  {>>>}

  {<<<}
  function getObjectFilename (var terminator: char): filenameType;

  var
    filename: filenameType;
    c : char;
    s : string;
    tempfile: fileListPtrType;

  begin
    filename := '';
    repeat
      { Get the next char that is part of a filename }
      repeat
        { Read the next char }
        read (cmdFile^.f, c);
        if newline then
          {<<<  if it's at the start of a line, handle switches and comments}
          begin
          newline := false;

          { Read sequence of comment/control lines }
          while ((c='!') OR (c='/') or (c = '@')) and not eof (cmdFile^.f) do { comment/control line}
            begin
            readln (cmdFile^.f,s);

            if c = '@' then
              begin
              {writeln( 'Line read is ', s );}
              while NOT (s[s.length] IN [ 'A' .. 'Z', 'a' .. 'z', '0' .. '9', '-', '_' ]) DO
                s.length := s.length - 1;

              writeln ('File read is ', s );

              new (tempfile);
              tempfile^.next := cmdFile;
              cmdFile := tempfile;
              reset (cmdFile^.f, s);
              writeln ('File opened is ', s);
              end

            else if (c='/') AND (pass = 1) then
              begin
              s := '/' + s;
              switchSettingsProcess (s);
              end;

            if NOT eof (cmdFile^.f) then
              read (cmdFile^.f, c);
            end;

          if eof (cmdFile^.f) then
            if cmdFile^.next <> NIL then
              BEGIN
              close (cmdFile^.f);
              cmdFile := cmdFile^.next;
              end;
          end;
          {>>>}
        if eoln (cmdFile^.f) then
           {<<<  eoln, eat objectEof lines}
           begin

           readln (cmdFile^.f);
           while eoln (cmdFile^.f) AND NOT eof (cmdFile^.f) DO
             readln (cmdFile^.f);

           newline := true;
           end;
           {>>>}
      until (NOT null(c)) OR eof (cmdFile^.f);

      if eof (cmdFile^.f) then
        if cmdFile^.next <> NIL then
          begin
          close (cmdFile^.f);
          cmdFile := cmdfile^.next;
          end;

      if (c <> '=') AND (c <> ',') AND (c <> '!') AND NOT null(c) then
        begin
        filename.length := filename.length+1;
        filename[filename.length] := c;
        end;
    until (c = ',') OR (c = '=') OR eof (cmdFile^.f);

    if eof (cmdFile^.f) then
      if cmdFile^.next <> NIL then
        begin
        close (cmdFile^.f);
        cmdFile := cmdFile^.next;
        end;

    terminator := c;
    getObjectFilename := filename;
    writeln ('getObjectFilename ', filename, ' terminated by ', c);
  end;
  {>>>}

  {<<<}
  procedure procId;

  var
    section: integer;
    coerce: record
      CASE integer of
        0: (ob: objrec);
        1: (id: idrec);
        end;

  begin
    topESD := 17;
    esdArray[0] := 0;  {unused esd value}

    coerce.ob := o;
    modName := coerce.id.modName;

    { we need to init these esd values, in case of zero length sections}
    if pass = 2 then
      begin
      if chat OR debug then
        writeln ('Pass 2 of ', modName,':');

      if modules then
        begin
        write (moduleFile, modName, ':');
        if files then
          begin
          {if fileId.length < 50 then
            write (moduleFile, pad(fileId, ' ', 50 ), ':' )
          else
          }
          write (moduleFile, fileId, ':' );
          end;
        end;

      for section := 0 TO 15 DO
        begin
        esdArray[section+1] := baseaddr[section]+sbase[section];
        esdSymbolArray [topESD] := NIL;
        outAddrArray[section+1] := esdArray[section+1];
        end;
      end

    else if chat OR debug then
      writeln ('Pass 1 of ', modName,':');
  end;
  {>>>}
  {<<<}
  procedure firstPass;

  var
    firstFile : boolean;

    {<<<}
    procedure processRecord;
    { first pass object record processor }

      {<<<}
      procedure procesd;

        {<<<}
        procedure doEsd;

        var
          ty : byte;
          section : integer;
          s : symbolNameType;
          b : boolean;
          i:integer;
          symbol : symbolPtr;

          {<<<}
          procedure addRef (s: symbolPtr; mod_name: symbolNameType);

          var
            reference: referencePtr;

          begin
            new (reference);
            reference^.next := s^.refList;
            reference^.symbolName := mod_name;
            s^.refList := reference;
          end;
          {>>>}

        begin
          symbol := nil;
          ty := getByte;
          section := ty MOD 16;
          ty := ty DIV 16;

          CASE ty of
            0 : bpos := bpos + 8;
            {<<<}
            1:
              begin { common area symbol }
              s := getSymbolName;
              i := getInt;
              b := findInsert (s, symbol, true);

              if debug then
                writeln ('Common data - section ', section:2,' ', s, ' length = ', hex(i,6,6));

              if xref then
                addRef (symbol, modName);

              if NOT symbol^.def then
                begin
                if b then numUndefinedSymbols := numUndefinedSymbols-1;
                symbol^.modName := modName;
                symbol^.section := section;
                symbol^.def := true;
                symbol^.comsize := i;
                if prevCommon <> nil then
                  prevCommon^.nextCommon := symbol
                else
                  commonHead := symbol;
                symbol^.nextCommon := nil;
                prevCommon := symbol;
                end

              else
                if (i<>symbol^.comsize) then
                  begin
                  if (NOT symbol^.flagged) AND (symbol^.comsize=-1) then
                    begin
                    showModName;
                    writeln ('Label ''', s, ''' is used double defined - ');
                    writeln ('as a common in module ''', modName, '''');
                    writeln (' and as an XDEF in module ''', symbol^.modName, '''');
                    symbol^.flagged := true;
                   end

                 else if check AND (NOT symbol^.flagged) then
                   begin
                   showModName;
                   writeln ('Common area size clash - common ''', s, '''');
                   writeln ('size in this module is ',hex(i,6,6), ' bytes');
                   writeln ('size in ''', symbol^.modName,''' is ', hex (symbol^.comsize,6,6), ' bytes');
                   symbol^.flagged := true;
                   end;

                 if (i>symbol^.comsize) AND (symbol^.comsize <> -1) then
                   begin
                   symbol^.modName := modName;
                   symbol^.comsize := i;
                   end;
                 end;
               end;
            {>>>}
            {<<<}
            2,3 :
              begin { section definition and allocation }
              i := getInt;
              if debug then
                writeln ('Section - ', section:2,' ', ' length = ', hex(i,6,6));

              sectbase[section] := sectbase[section]+i;
              if odd(sectbase[section]) then
                sectbase[section] := sectbase[section] + 1;
              end;
            {>>>}
            {<<<}
            4,5 :
              begin { symbol defintion }
              if ty = 5 then
                section := -1;
              s := getSymbolName;
              b := findInsert (s, symbol, true);

              { this isnt right yet, should fix it }
              if (symbol^.def) AND (NOT symbol^.flagged) then
                begin
                if symbol^.hist then { previously defined by history file }
                  begin
                  if chat then
                    writeln ('redefining ',s);
                  end
                else
                  doubledef(symbol)
                end

              else
                if b then
                  begin
                  if symbol^.hist then { previously defined by history file }
                    begin
                    if chat then
                      writeln ('redefining ',s);
                    end
                  else
                    numUndefinedSymbols := numUndefinedSymbols - 1;
                  end;

              symbol^.modName := modName;
              symbol^.section := section;
              symbol^.def := true;
              symbol^.addr := getInt + sectbase[section];
              end;
            {>>>}
            {<<<}
            6,7 :
              begin { symbol reference }
              if ty = 6 then
                begin
                showModName;
                writeln ('xref ',section);
                end;

              s := getSymbolName;
              b := findInsert (s, symbol, true);
              if xref then
                addRef (symbol, modName);
              if (NOT b) then
                numUndefinedSymbols := numUndefinedSymbols + 1;
              end;
            {>>>}
            {<<<}
            8,9 :
              begin
              showModName;
              writeln ('cl address');
              bpos := bpos + 5;
              end;
            {>>>}
            {<<<}
            10 :
              begin
              showModName;
              writeln ('cl addr common');
              bpos := bpos + 15;
              end;
            {>>>}
            end;
        end;
        {>>>}

      begin
        bpos := 2;
        while bpos < o.length DO
          doEsd;
      end;
      {>>>}
      {<<<}
      procedure proctxt;

      begin
      end;
      {>>>}
      {<<<}
      procedure proceom;

      begin
      end;
      {>>>}

    begin
      CASE o[1] of
        '1': procid;
        '2': procesd;
        '3': proctxt;
        '4': proceom;
        end;
    end;
    {>>>}

  begin
    pass := 1;
    basepos := startbase;

    firstFile := true;
    repeat
      filename := getObjectFilename (termchar);
      if termchar = '=' then
        begin
        if firstFile then
          commandRoot := filename
        else
          writeln ('You can''t put an ''='' THERE!');
        end
      else
        {<<<  process the file}
        begin
        ext := getExt (filename, defExt);

        if ext = '.his' then { history file of previous link }
          begin
          if usingHistory then
            writeln ('Can only use one history file, subsequent ones ignored')
          else
            begin
            startReadHis := currentTime;
            readHistory (filename);
            endReadHis := currentTime;
            end;

          usingHistory := TRUE;
          end

        else if ext = '.rx' then { text format .rx file }
          begin
          openTextIn (filename);
          repeat
            getTextRec (o);
            if o.length > 0 then
              processRecord;
          until eof (textObjectFile) ;

          closeTextIn;
          end

        else { normal .ro file }
          begin
          openIn (filename);

          repeat
            getRecord (o);
            if o.length > 0 then
              processRecord;
          until objectEof;

          closeIn;
          end;

        end;
        {>>>}

      firstFile := false;
      until eof (cmdFile^.f);

    endPass1 := currentTime;
  end;
  {>>>}
  {<<<}
  procedure secondPass;

    {<<<}
    procedure openOutput;

    begin
      opos := 0;
      inpacket := false;

      if bin then
        begin
        if download then
          begin
          rewrite (downloadTargetFile, 'target.txt');
          writeln ('Downloading binary file - target.txt');
          end;

        if out then
          begin
          rewrite (binaryFile, commandRoot + '.bin');
          if chat OR debug OR (NOT quiet) then
            writeln ('Making binary file ', commandRoot + '.bin');
          if logging then
            writeln (logFile, 'Making binary file ', commandRoot + '.bin');
          end;
        end

      else
        begin
        if download then
          begin
          rewrite (targetFile, 'target.txt');
          writeln ('Downloading SR file - target.txt');
          end;

        if out then
          begin
          rewrite (srFormatFile, commandRoot+'.sr');
          writeln ('Making SR file ', commandRoot + '.sr');
          end;
        end;
    end;
    {>>>}
    {<<<}
    procedure closeOutput;

    begin
      if inpacket then
        endpacket;

      sendStop;

      if bin then
        begin
        if download then
          close (downloadTargetFile);
        if out then
          close (binaryFile);
        end
      else
        begin
        if download then
          close (targetFile);
        if out then
          close (srFormatFile);
        end;
    end;
    {>>>}
    {<<<}
    procedure openModules;

    begin
    if modules then
      rewrite (moduleFile, commandRoot + '.MOD');
    end;
    {>>>}
    {<<<}
    procedure closeModules;

    begin
    if modules then
      close (moduleFile);
    end;
    {>>>}
    {<<<}
    procedure processRecord;
    { second pass object record processor }

      {<<<}
      procedure outputData;

      var
        c,pos : integer;

        {<<<}
        procedure srFormat (pos, len:integer);

        var
          b,cstart,i:integer;

          {<<<}
          procedure startpacket;

          var
            pktstart : string;
            plen : integer;

          begin
            cstart := codestart + pos*2;
            plen := len*2 + 4; {this happens to be right for both}

            if bin then
              BEGIN
              binbyte (esc);
              binbyte (0);
              wbyte (1);
              wbyte (mvr(plen));
              wbyte (plen MOD 256);
              wbyte (mvr(mvr(mvr(cstart))));
              wbyte (mvr(mvr(cstart)));
              wbyte (mvr(cstart));
              wbyte (cstart MOD 256);
              end

            else
              begin
              pktstart:='S2';
              sendsform (pktstart);
              wbyte (plen);
              wbyte (mvr(mvr(cstart)));
              wbyte (mvr(cstart));
              wbyte ((cstart MOD 256));
              end;
          end;
          {>>>}

        begin
          checksum := 0;
          startpacket;

          if bin then
            for i := 1 TO len DO
              begin
              b := int (uand (%x'FFFF', uint (codeArray[i+pos]))) DIV 256;
              if (b = esc) AND (escape = true) then
                begin
                oblock[opos] := b;
                opos := opos + 1;
                if opos > 511 then
                  begin
                  if out then write
                    (binaryFile,oblock);
                  if download then
                    write (downloadTargetFile, oblock);
                  opos := 0;
                  end;
                end;

              oblock[opos] := b;
              opos := opos + 1;
              if opos > 511 then
                begin
                if out then
                  write (binaryFile,oblock);
                if download then
                  write (downloadTargetFile,oblock);
                opos := 0;
                end;

              checksum := ord(uxor(uint(b),uint(checksum)));

              b := codeArray[i+pos] MOD 256;
              if (b = esc) AND (escape = true) then
                begin
                oblock[opos] := b;
                opos := opos + 1;
                if opos > 511 then
                  begin
                  if out then
                    write(binaryFile,oblock);
                  if download then
                    write (downloadTargetFile,oblock);
                  opos := 0;
                  end;
                end;

              oblock[opos] := b;
              opos := opos + 1;
              if opos > 511 then
                begin
                if out then
                  write(binaryFile,oblock);
                if download then
                  write(downloadTargetFile, oblock);
                opos := 0;
                end;
              checksum := ord (uxor (uint(b), uint (checksum)));
              end
          else
            for i := 1 TO len DO
              begin
              wbyte (mvr (codeArray[i+pos]));
              wbyte (codeArray[i+pos] MOD 256);
              end;

          endPacket;
        end;
        {>>>}

      begin
        c := codelen;

        pos := 0;
        while c > smax DO
          begin
          srFormat (pos, smax);
          pos := pos + smax;
          c := c - smax;
          end;

        if c > 0 then
          srFormat (pos, c);
      end;
      {>>>}
      {<<<}
      procedure procesd;

        {<<<}
        procedure doesd;

        var
          section : byte;
          esdType : byte;
          s : symbolNameType;
          b : boolean;
          patch :integer;
          i: integer;
          symbol : symbolPtr;
          r : resolvePtr;

        begin
          esdType := getByte;
          section := esdType MOD 16;
          esdType := esdType DIV 16;

          CASE esdType of
            {<<<}
            0:
              begin { no idea !! }
              bpos := bpos + 4;

              i := getInt;
              esdArray[topESD] := i;
              esdSymbolArray [topESD] := NIL;

              outAddrArray[topESD] := esdArray[topESD];
              topESD := topESD + 1;
              end;
            {>>>}
            {<<<}
            1:
              begin { common area symbol }
              s := getSymbolName;
              bpos := bpos + 4; {skip int}

              b := findInsert (s, symbol, false);
              if NOT b then
                begin
                showModName;
                writeln ('internal consistency check failure - lost symbol');
                end;

              esdArray[topESD] := symbol^.addr + baseaddr[symbol^.section];
              esdSymbolArray [topESD] := symbol;

              outAddrArray[topESD] := esdArray[topESD];
              topESD := topESD + 1;
              end;
            {>>>}
            {<<<}
            2,3 :
              begin { sectionion symbol }
              i := getInt;
              esdArray[section+1] := baseaddr[section] + sbase[section];
              esdSymbolArray [topESD] := NIL;

              outAddrArray[section+1] := esdArray[section+1];
              if modules then
                write (moduleFile,' ', section:2,':', hex (esdArray[section+1],6,6),'+', hex(i,6,6));

              sbase[section] := sbase[section] + i;

              if odd (sbase[section]) then
                sbase[section] := sbase[section] + 1;
              end;
            {>>>}
            {<<<}
            4,5 :
              if usingHistory then
                begin { symbol defintion, use to make patches on second pass }
                s := getSymbolName;
                b := findInsert (s, symbol, FALSE); { find it }

                if symbol^.resList <> nil then
                  begin
                  r := symbol^.resList;
                  repeat
                    begin
                    patch := symbol^.addr + baseaddr[symbol^.section] + r^.offset;
                    if debug then
                      writeln ('patching ',hex(r^.addr,6,6), ' with ',
                                           hex(patch-r^.offset,6,6), ' + ', hex(r^.offset,6,6));

                    codestart := r^.addr;
                    codeArray [1] := mvr (mvr (patch));
                    codeArray [2] := patch;
                    codelen := 2;
                    outputData;

                    r := r^.next;
                    end until r = nil;
                  end;

                bpos := bpos + 4; { skip past offset into module }
                end
              else
                bpos := bpos + 14; { skip past offset into module }
            {>>>}
            {<<<}
            6,7 :
              begin { symbol reference }
              s := getSymbolName;

              b := findInsert (s, symbol, false);
              if NOT b then
                begin
                showModName;
                writeln ('internal check failure - lost symbol');
                end;

              esdArray[topESD] := symbol^.addr + baseaddr[symbol^.section];
              esdSymbolArray [topESD] := symbol;

              outAddrArray[topesd] := esdArray[topESD];
              topESD := topESD + 1;
              end;
            {>>>}
            8,9 : bpos := bpos + 5;
            10  : bpos := bpos + 15;
            end;
        end;
        {>>>}

      begin
        bpos := 2;
        while bpos < o.length DO
          doesd;
      end;
      {>>>}
      {<<<}
      procedure proctxt;

      var
        bitmap, curresd: integer;

        {<<<}
        procedure procbyte;

        var
          longwd : boolean;
          offset,add,i,numesds,offsize : integer;
          thisesd,w:integer;
          flag : byte;

          {<<<}
          procedure adddata(w:integer);

          begin
            duffer := w = %x'4EBA';
            codelen := codelen + 1;
            codeArray[codelen] := w;
          end;
          {>>>}

        begin
          if bitmap >= 0 then
            begin
            adddata (mvl (ord (o[bpos])) + ord(o[bpos+1]));
            bpos := bpos+2;
            end

          else
            begin
            if duffer then
              begin
              showModName;
              writeln ('Warning - possible assembler foul-up');
              end;

            flag := getByte;
            numesds := flag DIV 32;
            offsize := flag MOD 8;
            {    writeln('num esds, ',numesds,'  offset size ',offsize);}
            longwd := ((flag DIV 8) MOD 2) = 1;

            add := 0;
            for i := 1 TO numesds DO
              begin
              thisesd := getByte;
              if thisesd > topESD then
                begin
                showModName;
                writeln(' assembler foul-up.. trying to use an undefined ESD : ' , thisesd);
                end;

              if odd(i) then
                add := add + esdArray[thisesd]
              else
                add := add - esdArray[thisesd];
              end;

            offset := 0;
            for i := 1 TO offsize DO offset := mvl(offset) + getByte;
            CASE offsize of
              0,4:;
              1: if offset > 127   then offset := int (uor (uint (offset),%X'FFFFFF00'));
              2: if offset > 32767 then offset := int (uor (uint (offset),%X'FFFF0000'));
              end;
            {    writeln('ofFSET ',hex(add,6,6),'+',hex(offset,6,6),'=',hex(add+offset,6,6));
            }
            add := add + offset;
            if numesds = 0 then
              begin
              if odd(offset) then
                begin
                showModName;
                writeln ('odd fix-up offset - assembler error .', offset, curresd);
                writeln ('>>', hex(codestart, 6, 6));
                offset := offset + 1;
                end;

              if codelen > 0 then
                outputData;

              outAddrArray[curresd] := outAddrArray[curresd] + codelen*2 + offset;
              codelen := 0;
              codestart := outAddrArray[curresd];
              end

            else  { numesd <> 0 }
              begin
              if NOT longwd then
                begin
                if (add > 32767) OR (add < -32768) then
                  begin
                  showModName;
                  writeln ('Long address generated into word location :', hex (add, 8, 8));
                  end;
                end;

              if esdSymbolArray [thisesd] <> NIL then { only need named symbols }
                if modName <> esdSymbolArray [thisesd]^.modName then { outside module }
                  begin
                  if history then
                    { address to be resolved LONGWORD only at present}
                    addRes (esdSymbolArray [thisesd], codestart + codelen*2, offset);

                  if debug then
                    writeln ('sym ', longwd,
                             ' ', thisesd:2,
                             ' ', esdSymbolArray [thisesd]^.symbolName,
                             ' ', hex (add,8,8), ' = ', hex (esdArray[thisesd]),
                             ' + ', hex (offset,4,4), ';', hex (offsize, 1, 1),
                             ' at ', hex (codestart + codelen * 2, 8, 8));
                  end;

              { generate resolved address }
              if longwd then
                adddata (mvr (mvr (add)));
              adddata (add);
              end;
            end;
          bitmap := bitmap*2;
        end;
        {>>>}

      begin
        bpos := 2;
        bitmap := getInt;

        codelen := 0;
        curresd := getByte;
        codestart := outAddrArray[curresd];

        while bpos < o.length DO
          procbyte;

        outputData;
        { dont forget convert to bytes}
        outAddrArray[curresd] := outAddrArray[curresd]+(codelen*2);
      end;
      {>>>}
      {<<<}
      procedure proceom;

      begin
        if modules then
          writeln (moduleFile);
      end;
      {>>>}

    begin
      CASE o[1] of
        '1': procid;
        '2': procesd;
        '3': proctxt;
        '4': proceom;
        end;
    end;
    {>>>}

  begin
    pass := 2;

    openoutput;
    openModules;

    { read .cmd file again }
    reset (cmdFile^.f);
    newline := true;

    { init sections }
    for i := 0 TO 15 DO
      sbase[i] := 0;

    repeat
      filename := getObjectFilename (termchar);
      if termchar <> '=' then
        begin
        ext := getExt (filename, defExt);
        if ext = '.his' then
          begin
          { do not reread history file }
          end
        else if ext = '.rx' then
          {<<<  .rx file}
          begin
          openTextIn (filename);

          repeat
            getTextRec (o);
            if o.length > 0 then
              processRecord;
          until eof (textObjectFile) ;

          closeTextIn;
          end
          {>>>}
        else
          {<<<  .ro file}
          begin
          openIn (filename);

          repeat
            getRecord (o);
            if o.length > 0 then
              processRecord;
          until objectEof;

          closeIn;
          end;
          {>>>}
        end;
      until eof (cmdFile^.f);

    closeModules;
    closeoutput;

    endPass2 := currentTime;
  end;
  {>>>}

  {<<<}
  procedure init;

  begin
    xref := false;
    symout := false;
    bin := false;
    check := false;
    bell := false;
    out := true;
    map := true;

    files := false;
    chat := FALSE;
    quiet := false;
    english := false;
    logging := false;
    history := FALSE;
    modules := false;
    escape := true;

    newline := true;
    usingHistory := FALSE;

    fileId := 'no file open.' ;

    { set up pointers for common area list }
    commonHead := nil;
    prevCommon := nil;

    defExt := '.ro';

    total := 0;
    numUndefinedSymbols := 0;

    for i := -1 TO 15 DO
      begin
      sectbase[i] := 0;
      userbase[i] := -1;  { set up user bases as not needed }
      end;

    for i := 0 TO maxHash DO
      hashTable[i] := nil;

    clearMilestone (startLink);
    clearMilestone (endPass1);
    clearMilestone (endPass2);
    clearMilestone (endLink);
    clearMilestone (endMapGen);
    clearMilestone (endHisGen);
    clearMilestone (endSymGen);
    clearMilestone (endSpaceAlloc);
    clearMilestone (endXrefGen);
    clearMilestone (startReadHis);
    clearMilestone (endReadHis);
  end;
  {>>>}
  {<<<}
  procedure reportHash;

  var
    depth, i, hash_used: integer;
    depth_used: array[ 0 .. 10 ] of integer;
    s_ptr: symbolPtr;

  begin
    hash_used := 0;

    for depth := 0 TO 10 DO
      depth_used[ depth ] := 0;

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        hash_used := hash_used + 1;
        depth := 0;
        s_ptr := hashTable[i];
        repeat
          depth := depth + 1;
          s_ptr := s_ptr^.nextSymbol;
        until s_ptr = nil;

        if depth > 10 then
          begin
          writeln ('Depth ', depth :0, ' too deep, rounding to 10' );
          depth := 10;
          end;
        depth_used[ depth ] := depth_used[ depth ] + 1;
        end;

    if chat OR debug then
      begin
      writeln (hash_used :0, ' out of ', maxHash:0, ' hash table entries used');
      for depth := 0 TO 10 DO
        if depth_used[depth] <> 0 then
          write (depth_used[depth]:0, ' @ ', depth:0, ',' );
      writeln;
      end;
  end;
  {>>>}

{ main }
{<<<}
begin
  init;

  { get command line }
  commandLen := -1;
  P_getcmdline (command, commandLen);
  writeln ('command ', command:commandLen, ' len:', commandLen:0);

  switchSettingsProcess (command);

  ext := getExt (command, '.cmd');
  commandRoot := command;
  commandRoot.length := command.length - ext.length;
  writeln ('ext:', ext, ' extLength:', ext.length:0, ' command:', command, ' commandRoot:', commandRoot);

  startLink := currentTime;
  startReadHis := currentTime;
  endReadHis := startReadHis;

  { open .cmd file }
  NEW (cmdFile);
  cmdFile^.next := NIL;
  if chat OR debug then
    writeln ('File given is ', command );
  reset (cmdFile^.f, command);
  fullFilename := command;
  writeln ('Linking from ', fullFilename);

  firstPass;
  allocCom;
  reportHash;

  if chat OR debug OR (NOT quiet) then
    {<<<  report symbolTable}
    begin
    writeln (numSymbols:5,' in symbol table');

    if logging then
      writeln (logFile, numSymbols:5,' in symbol table');
    end;
    {>>>}

  baseaddr[-1] := 0;      {set up base of absolute section}
  for i := 0 TO 15 DO
    {<<<  report section}
    begin
    if userbase[i] <> -1 then { put this section somewhere special}
      basepos := userbase[i];

    if sectbase[i] <> 0 then
      begin
      if NOT english then
        write ('Section ',i:2,' Start ',hex(basepos,6,6),' Length ', hex(sectbase[i],6,6));
      baseaddr[i] := basepos;
      basepos := basepos+sectbase[i];
      if NOT english then
        writeln (' Finish  ',hex(basepos,6,6));
      end;
    end;
    {>>>}

  if english then
    {<<<  report section usage nicely}
    begin
    writeln;
    if sectbase[8] <> 0 then
      begin
      writeln ('Size of P                 (8)  = ', sectbase[8]:8, ' bytes'); total := total + sectbase[8];
      end;

    if sectbase[9] <> 0 then
      begin
      writeln ('Size of HELP              (9)  = ', sectbase[9]:8, ' bytes');
      total := total + sectbase[9];
      end;

    if sectbase[12] <> 0 then
      begin
      writeln ('Size of error messages   (12)  = ', sectbase[12]:8, ' bytes');
      total := total + sectbase[12];
      end;

    if sectbase[13] <> 0 then
      begin
      writeln ('Size of code & constants (13)  = ', sectbase[13]:8, ' bytes');
      total := total + sectbase[13];
      end;

    if sectbase[14] <> 0 then
      begin
      writeln ('Size of diagnostic block (14)  = ', sectbase[14]:8, ' bytes');
      total := total + sectbase[14];
      end;

    if sectbase[15] <> 0 then
      begin
      writeln ('Size of global variables (15)  = ', sectbase[15]:8, ' bytes');
      total := total + sectbase[15];
      end;

    writeln ('Total size                     = ', total:8, ' bytes');
    end;
    {>>>}

  overlapCheck;
  endSpaceAlloc := currentTime;

  if numUndefinedSymbols <> 0 then
    {<<<  report undefined symbols}
    begin
    writeln('Number of undefined symbols:- ', numUndefinedSymbols);

    if logging then
      writeln (logFile, 'Number of undefined symbols:- ', numUndefinedSymbols);

    checkUndefinedSymbols;
    end;
    {>>>}

  if bin then
    smax := 512 { randomly large number! }
  else
    smax := 16; { s-format max line size }

  if modules OR out OR download then
    secondPass;

  {<<<  histoy}
  if history then
    dumpHistory;
  endHisGen := currentTime;
  {>>>}
  {<<<  symbolTable}
  if symout then
    dumpSymbols;
  endSymGen := currentTime;
  {>>>}
  {<<<  map}
  if map then
    dumpSymbolMap;
  endMapGen := currentTime;
  {>>>}
  {<<<  xref}
  if xref then
    dumpXreferences;
  endXrefGen := currentTime;
  {>>>}

  if bell then for i := 1 TO 10 DO
    write (chr(7));
  writeln;
  endLink := currentTime;

  if chat OR debug OR (NOT quiet) then
    {<<<  report timings}
    begin
    writeln ('Link started           ', startLink.timeOfDay);

    showMilestone ('Pass 1                 ', endPass1,startLink);

    if startReadHis.millTime <> endReadHis.millTime then
      showMilestone ('Reading history file   ', endReadHis, startReadHis );

    showMilestone ('Space allocation       ', endSpaceAlloc,endPass1);
    showMilestone ('Pass 2                 ', endPass2,endSpaceAlloc);

    if history then
      showMilestone ('.HIS generation        ', endHisGen, endPass2);
    if symout then
      showMilestone ('.SYM generation        ', endSymGen, endHisGen);
    if map then
      showMilestone ('.MAP generation        ', endMapGen, endSymGen);
    if xref then
      showMilestone ('.XRF generation        ', endXrefGen, endMapGen);
    showMilestone ('Link ended             ', endLink, startLink);

    writeln;
    writeln ('total CPU time:- ', (endLink.millTime - startLink.millTime) / 1000:7:2);
    end;
    {>>>}
  if english then
    {<<<  report timigs nicely}
    begin
    date (datestring);
    writeln;
    writeln ('Link started ', startLink.timeOfDay, ' ', datestring);
    writeln ('Link ended   ', endLink.timeOfDay, ' ', datestring);
    end;
    {>>>}

  if logging then
    closeloggingFile;
end.
{>>>}
