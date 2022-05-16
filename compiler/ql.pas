{ ql.pas }
PROGRAM ql (input,output);
{<<<}
const
  maxStringLength = 100;

  maxHash = 4095;

  esc = 27;
  startBase = %x'400';

  { probably chosen to match ethernet download packet size }
  bytesPerFileRec$ = 1536;
  recsPerFileRec$ = {102} (bytesPerFileRec$ - 4) DIV 15;
{>>>}
{<<<}
type
  byte = 0..255;
  word = -32768..32767;

  objBlockType = packed array [0..255] of byte;
  {<<<}
  objRecordType = packed record
    length: integer;
    block: objBlockType;
    end;
  {>>>}

  binBlockType = packed array [0..511] of byte;

  symbolNameType = packed array [1..10] of char;
  {<<<}
  idRecord = packed record
    rlen    : word;
    rtype   : char;
    modName : symbolNameType;
    end;
  {>>>}

  referencePtr = ^referenceType;
  {<<<}
  referenceType = packed record
    next : referencePtr;
    symbolName : symbolNameType;
    end;
  {>>>}

  resolvePtr = ^resolveType;
  {<<<}
  resolveType = packed record
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

  fileListPtr = ^fileListType;
  {<<<}
  fileListType = record
    next : fileListPtr;
    f : text;
    end;
  {>>>}

  {<<<}
  milestoneType = record
    intTime   : integer;
    millTime  : integer;
    timeOfDay : packed array [1..11] of char;
    end;
  {>>>}

  { history }
  historyType = ($historyObj, $historySymbol, $historyRef);
  {<<<}
  historyRecordType = record
    CASE historyType : historyType of
      $historyObj    : (obj_addr : integer;);
      $historySymbol : (symbolAddr : integer;
                        symbolName : symbolNameType;);
      $historyRef    : (refAddr: integer;
                        refOffset : integer;);
    end;
  {>>>}

  {<<<}
  fileHistoryRecordType = record CASE boolean of
    true : (numRecs : integer;
            recs    : ARRAY [1 .. recsPerFileRec$] of historyRecordType;);
    false: (dummy   : packed array [1 .. bytesPerFileRec$] of char);
    end;
  {>>>}
  historyFileType = FILE of fileHistoryRecordType;
{>>>}
{<<<}
var
  { command line }
  cmd: packed array [0..100] of char;
  cmdLen: word;
  cmdString: string;
  cmdFilenameString: string;
  cmdFileRootString: string;
  cmdFileExtString: string;

  curFilenameString: string;
  fileIdString: string;

  { files }
  cmdFile: fileListPtr;
  objFile : FILE of objBlockType;
  textObjFile: text;
  binaryFile: FILE of binBlockType;
  downloadTargetFile: FILE of binBlockType;
  targetFile: text;
  srFormatFile: text;
  logFile: text;
  moduleFile: text;

  smax, checksum, pass: integer;
  opos, bpos: integer;

  { obj file input }
  objEof: boolean;
  objBlockIndex: integer;
  objBlock: objBlockType;

  objRecord: objRecordType;

  { switches }
  modules, download, check, bell, xref, map, bin, out, symout: boolean;
  chat, debug, logging, friendly, quiet, files, history, escape: boolean;

  duffer: boolean;
  newline: boolean;
  inpacket: boolean;
  usingHistory: boolean;

  prevCommon: symbolPtr;
  commonHead: symbolPtr;

  { symbols }
  numSymbols: integer;
  numUndefinedSymbols: integer;
  hashTable: array [0..maxHash] of symbolPtr;

  { sections }
  codestart, codelen: integer;
  topESD: integer;
  userbase, sbase, sectbase, baseaddr: array [-1..15] of integer; {element -1 is the ABS section}
  esdArray: array [0..255] of integer;
  esdSymbolArray: array [0..255] of symbolPtr;
  outAddrArray: array [0..255] of integer;
  codeArray: array [1..64] of integer;

  { timing }
  startLinkMilestone, endPass1Milestone, endPass2Milestone, endLinkMilestone: milestoneType;
  endMapGenMilestone, endHisGenMilestone, endSymGenMilestone: milestoneType;
  endSpaceAllocMilestone, endXrefGenMilestone: milestoneType;
  startReadHisMilestone, endReadHisMilestone: milestoneType;

  modName: symbolNameType;

  { .bin file output }
  binBlock: binBlockType;

  i, total, basepos: integer;
  datestring: packed array [1..11] of CHAR;
{>>>}

  {<<<  bit utils}
  {<<<}
  function ior (i1, i2: integer): integer;

  begin
    ior := ord (uor (uint (i1), uint (i2)));
  end;
  {>>>}
  {<<<}
  function ixor (i1, i2: integer): integer;

  begin
    ixor := ord (uxor (uint(i1), uint(i2)));
  end;
  {>>>}
  {<<<}
  function iand (i1, i2: integer): integer;

  begin
    iand := ord (uand (uint (i1), uint (i2)));
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
      mvr := (i DIV 256) mod (%x'1000000');
  end;
  {>>>}
  {>>>}
  {<<<  char utils}
  {<<<}
  function isNull (ch: char): boolean;

  begin
    isNull := (ch = chr(13)) OR (ch = chr(10)) OR (ch = ' ') OR (ch = chr(9)) OR (ch = chr(0));
  end;
  {>>>}
  {<<<}
  function isDigit (ch: char): boolean;

  begin
    isDigit := (ch >= '0') and (ch <= '9');
  end;
  {>>>}
  {<<<}
  function isUpper (ch: char): boolean;

  begin
    isUpper := (ch >= 'A') and (ch <= 'Z');
  end;
  {>>>}
  {<<<}
  function isAlpha (ch: char): boolean;

  begin
    isAlpha := ((ch >= 'a') and (ch <= 'z')) or ((ch >= 'A') and (ch <= 'Z'));
  end;
  {>>>}
  {<<<}
  function isAlphaDigit (ch: char): boolean;

  begin
    isAlphaDigit := isDigit (ch) or isAlpha (ch) or (ch = '_');
  end;
  {>>>}

  {<<<}
  function toHex (ch: char): integer;

  begin
    if (ch >= '0') and (ch <= '9') then
      toHex := ord(ch) - ord('0')
    else if (ch >= 'a') and (ch <= 'f') then
      toHex := ord(ch) - ord('a') + 10
    else if (ch >= 'A') and (ch <= 'F') then
      toHex := ord(ch) - ord('A') + 10
    else
      begin
      writeln ('Duff char ''', ch,'''when hex char expected!');
      toHex := 0;
      end;
  end;
  {>>>}
  {<<<}
  function toLower (ch: char): char;

  begin
    if isUpper (ch) then
      toLower := chr(ord(ch) + ord('a') - ord('A'))
    else
      toLower := ch;
  end;
  {>>>}
  {>>>}
  {<<<}
  procedure subString (var t: string; s: string; start, span: integer);
  { Substring (T,S,Start,Span) - the substring of string S defined by Start, Span is assigned to the target string T }

  var
    i, slen: integer;

  begin
    t = '';

    if span < 0 then
      begin
      span := -span;
      start := start - span
      end;

    if start < 1 then
      begin
      span := span + start - 1;
      start := 1
      end;

    if start + span > s.length + 1 then
      span := slen - start + 1;

    for i := 1 to span do
      t := t + s[start + i - 1];
  end;
  {>>>}
  {<<<  file utils}
  {<<<}
  procedure getFileStrings (filename: string; defaultExt: string;
                            var root: string; var ext: string; var fullFilename: string);

  var
  i: integer;

  begin
    writeln ('getFileStrings string:', filename, ' len:', filename.length:0,
             ' ext:', defaultExt, ' len:', defaultExt.length:0);

    { search backwards, to find first non-alpha char is a '.' }
    i := filename.length;
    while (i > 0) AND isAlphaDigit (filename[i]) DO
      i := i - 1;

    if (i > 1) and (filename[i] = '.') then
      begin
      writeln ('has ext:', i:0, 'of:', filename.length);
      subString (root, filename, 1, i - 1);
      subString (ext, filename, i, filename.length - i + 1);
      fullFilename = filename;
      end
    else
      begin
      writeln ('no ext:', i:0, 'of:', filename.length);
      root := filename;
      ext = defaultExt;
      fullFilename := filename;
      concatenateString (fullFilename, defaultExt);
      end;
  end;
  {>>>}

  { logging file }
  {<<<}
  procedure openloggingFile;

  var
    filenameString: string;

  begin
    filenameString := cmdFileRootString;
    concatenateString (filenameString, '.log');
    rewrite (logFile, filenameString);
    writeln (logFile, 'Linking from ', cmdFilenameString);
  end;
  {>>>}
  {<<<}
  procedure closeloggingFile;

  var
    total, i:integer;
    datestring: packed array [1..11] of CHAR;

  begin
    if friendly then
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
    writeln (logFile, 'Link started ', startLinkMilestone.timeOfDay, ' ', datestring);
    writeln (logFile, 'Link ended   ', endLinkMilestone.timeOfDay, ' ', datestring);
    writeln (logFile, 'total CPU time:- ',(endLinkMilestone.millTime - startLinkMilestone.millTime)/1000:7:2);

    close (logFile);
  end;
  {>>>}

  { .ro obj file }
  {<<<}
  procedure openin (filenameString: string);

  begin
    cmdFilenameString := filenameString;
    reset (objFile, filenameString);

    read (objFile, objBlock);
    objBlockIndex := 0;

    objEof := false;
  end;
  {>>>}
  {<<<}
  procedure getRecord (var objRecord: objRecordType);
  { .ro files are 256 byte fixed size blocks. Within these blocks, records are
    packed end to end i.e. one record can span a block boundary. Each record
    conisits of a single byte <n> followed by <n> data bytes
  }
  var
    i: integer;
    numBytes: integer;

  begin
    { read objRecord length from objBlock }
    objRecord.length := objBlock[objBlockIndex];
    objBlockIndex := objBlockIndex + 1;

    if objRecord.length < (256 - objBlockIndex) then
      numBytes := objRecord.length
    else
      numBytes := 255 - objBlockIndex;

    { copy from objBlock to objRecord }
    for i := 1 TO numBytes DO
      begin
      objRecord.block[i-1] := chr(objBlock[objBlockIndex]);
      objBlockIndex := objBlockIndex + 1;
      end;

    if (objBlockIndex > 256) then
      begin
      { need new objBlock }
      if eof (objFile) then
        objEof := true
      else
        begin
        { read next objBlock from objFile }
        read (objFile, objBlock);
        objBlockIndex := 0;

        { if more objRecord bytes needed, copy from latest objBlock to rest of objRecord }
        for i := numBytes+1 TO objRecord.length DO
          begin
          objRecord.block[i] := objBlock[objBlockIndex];
          objBlockIndex := objBlockIndex + 1;
          end;
        end;
      end;
  end;
  {>>>}
  {<<<}
  procedure closeIn;

  begin
    close (objFile);
  end;
  {>>>}

  { .rx obj file }
  {<<<}
  procedure openTextIn (filenameString: string);

  begin
    cmdFilenameString := filenameString;
    reset (textObjFile, filenameString);
  end;
  {>>>}
  {<<<}
  procedure getTextRec (var o: objRecordType);
  { .rx files are a text version of the .ro file. Each record is a single line
    of text, written out as hex characters i.e. 2 characters per byte. The record
    length is derived from the bytes on the line. This format is provided to allow
    .rx files to be easily ported from other systems e.g. Unix
  }
  var
    bytes, i: integer;
    textObjRecord: string;

  begin
    readln (textObjFile, textObjRecord);

    bytes := textObjRecord.length DIV 2;
    objRecord.length := bytes;
    for i := 1 TO bytes DO
      objRecord.block[i] := chr ((toHex (textObjRecord[i*2-1]) * 16) + toHex (textObjRecord[i*2]));
  end;
  {>>>}
  {<<<}
  procedure closeTextIn;

  begin
    close (textObjFile);
  end;
  {>>>}

  { .bin binary output file }
  {<<<}
  procedure openOutput;

  var
    filenameString: string;

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
        filenameString := cmdFileRootString;
        concatenateString (filenameString, '.bin');
        rewrite (binaryFile, filenameString);
        if chat OR debug OR (NOT quiet) then
          writeln ('Making binary file ', filenameString);
        if logging then
          writeln (logFile, 'Making binary file ', filenameString);
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
        filenameString := cmdFileRootString;
        concatenateString (filenameString, '.sr');
        rewrite (srFormatFile, filenameString);
        writeln ('Making SR file ', filenameString);
        end;
      end;
  end;
  {>>>}
  {<<<}
  procedure binByte (b: byte);

  begin
    binBlock[opos] := b;
    opos := opos + 1;

    if opos > 511 then
      begin
      if out then
        write (binaryFile, binBlock);
      if download then
        write (downloadTargetFile, binBlock);

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

  { dump to file }
  {<<<}
  procedure dumpSymbols;

  var
    i:integer;
    bl: objBlockType;
    symbol: symbolPtr;
    symbolTableFile: file of objBlockType;
    filenameString: string;

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
      intString: string;
      i: integer;

    begin
      for i := 4 downto 1 DO
        begin
        intString[i] := chr (val);
        val := mvr (val);
        end;

      binInt := intString;
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

    filenameString := cmdFileRootString;
    concatenateString (filenameString, '.sym');
    rewrite (symbolTableFile, filenameString);

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
    filenameString: string;

  begin
    filenameString := cmdFileRootString;
    concatenateString (filenameString, '.map');
    rewrite (map_file, filenameString);

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
    filenameString: string;

  begin
    filenameString := cmdFileRootString;
    concatenateString (filenameString, '.xrf');
    rewrite (ref_file, filenameString);

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
          writeln (ref_file,'Not referenced ANYWHERE!');

        writeln
          (ref_file,'--------------------------------------------------------------------------');
        s_ptr := s_ptr^.nextSymbol;
        until s_ptr = nil;
      end;

    close (ref_file);
  end;
  {>>>}
  {<<<}
  procedure dumpHistory;
  { disp history and readHistory must have match in file format }

  var
    i, rescount: integer;
    symbol: symbolPtr;
    r : resolvePtr;
    historyFile : historyFileType;
    historyRecord : historyRecordType;
    fileHistoryRecord : fileHistoryRecordType;
    filenameString: string;

    {<<<}
    procedure writeHistoryFile (historyRecord: historyRecordType);

    begin
      fileHistoryRecord.numRecs := fileHistoryRecord.numRecs + 1;
      fileHistoryRecord.recs[fileHistoryRecord.numRecs] := historyRecord;

      if fileHistoryRecord.numRecs = recsPerFileRec$ then
        begin
        Write (historyFile, fileHistoryRecord);
        fileHistoryRecord.numRecs := 0;
        end;
    end;
    {>>>}

  begin
    filenameString := cmdFileRootString;
    concatenateString (filenameString, '.his');
    rewrite (historyFile, filenameString);
    fileHistoryRecord.numRecs := 0;

    historyRecord.historyType := $historyObj;
    historyRecord.obj_addr := basepos;

    { Write (historyFile, historyRecord); }
    writeHistoryFile (historyRecord);

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        symbol := hashTable[i];

        repeat
          begin
          if symbol^.comsize = -1 then { dont dump commons in history }
            begin
            historyRecord.historyType := $historySymbol;
            historyRecord.symbolAddr := symbol^.addr+baseaddr[symbol^.section];
            historyRecord.symbolName := symbol^.symbolName;
            {historyRecord.mod_name := symbol^.modName;}
            {Write (historyFile, historyRecord);}
            writeHistoryFile (historyRecord);

            if symbol^.resList <> nil then
              begin
              r := symbol^.resList;
              repeat
                begin
                historyRecord.historyType := $historyRef;
                historyRecord.refAddr := r^.addr;
                historyRecord.refOffset := r^.offset;
                {Write (historyFile, historyRecord);}
                writeHistoryFile (historyRecord);
                r := r^.next;
                end until r = nil;
              end;

            end;

          symbol := symbol^.nextSymbol;
          end until symbol = nil;

        end;

    { Send the last one }
    if fileHistoryRecord.numRecs > 0 then
      Write (historyFile, fileHistoryRecord);

    close (historyFile);
  end;
  {>>>}

  { .mod module file }
  {<<<}
  procedure openModules;

  var
    filenameString: string;

  begin
  if modules then
    begin
    filenameString := cmdFileRootString;
    concatenateString (filenameString, '.sym');
    rewrite (moduleFile, filenameString);
    end;
  end;
  {>>>}
  {<<<}
  procedure closeModules;

  begin
  if modules then
    close (moduleFile);
  end;
  {>>>}
  {>>>}

  {<<<}
  function getByte: byte;

  begin
    getByte := objRecord.block[bpos];
    bpos := bpos + 1;
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
  function getMilestone: milestoneType;

  var
    ms: milestoneType;
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

    getMilestone := ms;
  end;
  {>>>}
  {<<<}
  procedure clearMilestone (var ms: milestoneType);

  begin
    ms.millTime := 0;
    ms.intTime := 0;
    ms.timeOfDay := '           ';
  end;
  {>>>}
  {<<<}
  procedure showMilestone (s: string; milestone1, milestone2: milestoneType);

  var
    i, diff, cc, ss, mm, hh: integer;
    timeString: packed array [1..11] of char;

  begin
    diff := milestone1.intTime - milestone2.intTime;

    cc := diff MOD 100;
    diff := diff DIV 100;
    ss := diff MOD 60;

    diff := diff DIV 60;
    mm := diff MOD 60;

    diff := diff DIV 60;
    hh := diff MOD 60;

    write (s);
    write (milestone1.timeOfDay,' ',(milestone1.millTime-milestone2.millTime) / 1000:7:2);
    write (hh:2, ':', mm:2, ':', ss:2, '.', cc:2 );

    if endLinkMilestone.millTime - startLinkMilestone.millTime > 0 then
      write ('  ', ((milestone1.millTime - milestone2.millTime) * 100) /
                    (endLinkMilestone.millTime - startLinkMilestone.millTime):7:2,'%');
    writeln;
  end;
  {>>>}
  {<<<}
  procedure showModName;

  begin
    writeln ('in module:', modName, ' from file:', curFilenameString);
    if logging then
      writeln (logFile, 'in module:', modName, ' from file:', curFilenameString);
  end;
  {>>>}

  {<<<}
  function findInsert (var symbolName: symbolNameType; var symbol: symbolPtr; ins: boolean): boolean;
  var
    found : boolean;
    hash : integer;

    {<<<}
    procedure forceUpper (var s: symbolNameType);

    var
      i:integer;

    begin
      for i := 1 TO 10 DO
        if (s[i] >= 'a') AND (s[i] <= 'z') then
          s[i] := chr (ord (s[i]) + ord('A') - ord('a'));
    end;
    {>>>}

  begin
    forceUpper (symbolName);

    hash := symbolHash (symbolName);
    symbol := hashTable[hash];

    found := false;
    while (NOT found) AND (symbol <> nil) DO
      begin
      if symbol^.symbolName = symbolName then
        found := true
      else
        symbol := symbol^.nextSymbol;
      end;

    findInsert := found;

    if (NOT found) AND ins then
      begin
      numSymbols := numSymbols + 1;
      new (symbol);
      symbol^.nextSymbol := hashTable[hash];
      hashTable[hash] := symbol;

      symbol^.def := false;
      symbol^.used := true;
      symbol^.flagged := false;
      symbol^.symbolName := symbolName;
      symbol^.modName := modName;
      symbol^.comsize := -1;
      symbol^.refList := nil;
      symbol^.resList := nil;
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

    writeln ('Doubly defined label  ''', s^.symbolName, '''');
    writeln ('Previously defined in module ''', s^.modName, '''');

    if logging then
      begin
      writeln (logFile, 'Doubly defined label  ''', s^.symbolName, '''');
      writeln (logFile, 'Previously defined in module ''', s^.modName, '''');
      end;

    s^.flagged := true;
  end;
  {>>>}
  {<<<}
  procedure readHistory (filename: string);
  { disp history and readHistory must have match in file format }

  var
    symbol: symbolPtr;
    resolve: resolvePtr;

    fileHistoryRecord: fileHistoryRecordType;
    historyFile: historyFileType;

  begin
    symbol := nil;

    reset (historyFile, filename);

    while NOT eof (historyFile) DO
      begin
      Read (historyFile, fileHistoryRecord);

      for i := 1 TO fileHistoryRecord.numRecs DO
        WITH fileHistoryRecord.recs[i] DO
          CASE historyType of
            $historyObj :
              basepos := obj_addr;

            $historySymbol :
              begin
              if findInsert (symbolName, symbol, true) then;
              symbol^.hist := TRUE;
              symbol^.modName := 'patched!!!';
              symbol^.section := -1;
              symbol^.def := true;
              symbol^.addr := symbolAddr;
              end;

            $historyRef :
              begin
              new (resolve);
              resolve^.next := symbol^.resList;
              resolve^.addr := refAddr;
              resolve^.offset := refOffset;
              symbol^.resList := resolve;
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
    lineBuffer: packed array [1..maxStringLength+1] of char; {extra byte to stop bound overflow!}

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
      for i := 1 TO switchLen DO
        lineBuffer[i] := toLower (lineBuffer[i]);

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
          until (c='/') OR (isNull (c)) OR (pos >= switchLen);

        if (tla[1] = 'o') AND (tla[2] >= '0') AND (tla[2] <= '9') then
          {<<<  set section startAddress}
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
              userbase[section] := 16 * userbase[section] + toHex (lineBuffer[i]);
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
          setSwitch (download) { download to target}
        else if tla = 'out' then
          setSwitch (out) { generate any output at all!}
        else if tla = 'cha' then
          setSwitch (chat) { generate loads of output }
        else if tla = 'qui' then
          setSwitch (quiet) { generate minimum output}
        else if tla = 'eng' then
          setSwitch (friendly) { say understandable things}
        else if tla = 'log' then
          {<<<  generate .log file}
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
          setSwitch (check) { check all possible grubbies }
        else if tla = 'esc' then
          setSwitch (escape) { replace all 1B's in code with 1B1B }
        else
          writeln ('Unknown switch :', tla);
      until (pos >= switchLen) OR (isNull (c));
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
  function getObjFilename (var terminator: char): string;

  var
    filename: string;
    c: char;
    s: string;
    tempfile: fileListPtr;

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
              while NOT (s.length IN [ 'A' .. 'Z', 'a' .. 'z', '0' .. '9', '-', '_' ]) DO
                s.length = s.length - 1;
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
           {<<<  eoln, eat objEof lines}
           begin

           readln (cmdFile^.f);
           while eoln (cmdFile^.f) AND NOT eof (cmdFile^.f) DO
             readln (cmdFile^.f);

           newline := true;
           end;
           {>>>}
      until (NOT isNull (c)) OR eof (cmdFile^.f);

      if eof (cmdFile^.f) then
        if cmdFile^.next <> NIL then
          begin
          close (cmdFile^.f);
          cmdFile := cmdfile^.next;
          end;

      if (c <> '=') AND (c <> ',') AND (c <> '!') AND NOT isNull (c) then
        appendChar (filename, c);
    until (c = ',') OR (c = '=') OR eof (cmdFile^.f);

    if eof (cmdFile^.f) then
      if cmdFile^.next <> NIL then
        begin
        close (cmdFile^.f);
        cmdFile := cmdFile^.next;
        end;

    terminator := c;
    getObjFilename := filename;
    writeln ('getObjFilename ', filename, ' terminated by ', c);
  end;
  {>>>}

  {<<<}
  procedure processModuleId;

  var
    section: integer;
    coerce: record
      CASE integer of
        0: (ob: objRecordType);
        1: (id: idRecord);
        end;

  begin
    topESD := 17;
    esdArray[0] := 0;  {unused esd value}

    coerce.ob := objRecord;
    modName := coerce.id.modName;

    { we need to init these esd values, in case of zero length sections}
    if pass = 2 then
      begin
      if chat OR debug then
        writeln ('Pass2 of ', modName,':');

      if modules then
        begin
        write (moduleFile, modName, ':');
        if files then
          begin
          {if fileId.length < 50 then
            write (moduleFile, pad(fileId, ' ', 50 ), ':' )
          else
          }
          write (moduleFile, fileIdString, ':' );
          end;
        end;

      for section := 0 TO 15 DO
        begin
        esdArray[section+1] := baseaddr[section] + sbase[section];
        esdSymbolArray [topESD] := NIL;
        outAddrArray[section+1] := esdArray[section+1];
        end;
      end

    else if chat OR debug then
      writeln ('Pass1 of ', modName,':');
  end;
  {>>>}
  {<<<}
  procedure pass1;

  var
    termchar: char;
    objFilename, objRootString, objExtString, objFullFilenameString: string;

    {<<<}
    procedure processRecord;
    { pass1 obj record processor }

      {<<<}
      procedure processESD;

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
        while bpos < objRecord.length DO
          doEsd;
      end;
      {>>>}
      {<<<}
      procedure processText;

      begin
      end;
      {>>>}
      {<<<}
      procedure processEOM;

      begin
      end;
      {>>>}

    begin
      CASE chr(objRecord.block[0]) of
        '1': processModuleId;
        '2': processESD;
        '3': processText;
        '4': processEOM;
        end;
    end;
    {>>>}

  begin
    pass := 1;

    basepos := startBase;

    repeat
      objFilename := getObjFilename (termchar);
      {<<<  process the file}
      begin
      getFileStrings (objFilename, '.ro', objRootString, objExtString, objFullFilenameString);

      if objExtString = '.his' then { history file of previous link }
        begin
        if usingHistory then
          writeln ('Only one history file, ignoring ', objFullFilenameString)
        else
          begin
          startReadHisMilestone := getMilestone;
          readHistory (objFullFilenameString);
          endReadHisMilestone := getMilestone;
          end;
        usingHistory := TRUE;
        end

      else if objExtString = '.rx' then
        begin
        openTextIn (objFullFilenameString);

        repeat
          getTextRec (objRecord);
          if objRecord.length > 0 then
            processRecord;
        until eof (textObjFile) ;

        closeTextIn;
        end

      else if objExtString = '.ro' then
        begin
        openIn (objFullFilenameString);

        repeat
          getRecord (objRecord);
          if objRecord.length > 0 then
            processRecord;
        until objEof;

        closeIn;
        end;

      end;
      {>>>}
      until eof (cmdFile^.f);

    endPass1Milestone := getMilestone;
  end;
  {>>>}
  {<<<}
  procedure pass2;

  var
    termchar: char;
    section: integer;
    objFilename, objRootString, objExtString, objFullFilenameString: string;

    {<<<}
    procedure processRecord;
    { second pass obj record processor }

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
                binBlock[opos] := b;
                opos := opos + 1;
                if opos > 511 then
                  begin
                  if out then write
                    (binaryFile, binBlock);
                  if download then
                    write (downloadTargetFile, binBlock);
                  opos := 0;
                  end;
                end;

              binBlock[opos] := b;
              opos := opos + 1;
              if opos > 511 then
                begin
                if out then
                  write (binaryFile, binBlock);
                if download then
                  write (downloadTargetFile, binBlock);
                opos := 0;
                end;

              checksum := ord(uxor(uint(b),uint(checksum)));

              b := codeArray[i+pos] MOD 256;
              if (b = esc) AND (escape = true) then
                begin
                binBlock[opos] := b;
                opos := opos + 1;
                if opos > 511 then
                  begin
                  if out then
                    write(binaryFile, binBlock);
                  if download then
                    write (downloadTargetFile, binBlock);
                  opos := 0;
                  end;
                end;

              binBlock[opos] := b;
              opos := opos + 1;
              if opos > 511 then
                begin
                if out then
                  write(binaryFile, binBlock);
                if download then
                  write(downloadTargetFile, binBlock);
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
      procedure processESD;

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
        while bpos < objRecord.length DO
          doesd;
      end;
      {>>>}
      {<<<}
      procedure processText;

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
            adddata (mvl (ord (objRecord.block[bpos])) + ord(objRecord.block[bpos+1]));
            bpos := bpos + 2;
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

        while bpos < objRecord.length DO
          procbyte;

        outputData;
        { dont forget convert to bytes}
        outAddrArray[curresd] := outAddrArray[curresd]+(codelen*2);
      end;
      {>>>}
      {<<<}
      procedure processEOM;

      begin
        if modules then
          writeln (moduleFile);
      end;
      {>>>}

    begin
      CASE chr(objRecord.block[0]) of
        '1': processModuleId;
        '2': processESD;
        '3': processText;
        '4': processEOM;
        end;
    end;
    {>>>}

  begin

    openoutput;
    openModules;

    { read .cmd file again }
    reset (cmdFile^.f);
    newline := true;

    { init sections }
    for section := 0 TO 15 DO
      sbase[section] := 0;

    repeat
      objFilename := getObjFilename (termchar);
      if termchar <> '=' then
        begin
        getFileStrings (objFilename, '.ro', objRootString, objExtString, objFullFilenameString);
        if objExtString = '.his' then
          begin
          end
        else if objExtString = '.rx' then
          {<<<  .rx file}
          begin
          openTextIn (objFullFilenameString);

          repeat
            getTextRec (objRecord);
            if objRecord.length > 0 then
              processRecord;
          until eof (textObjFile) ;

          closeTextIn;
          end
          {>>>}
        else if objExtString = '.ro' then
          {<<<  .ro file}
          begin
          openIn (objFullFilenameString);

          repeat
            getRecord (objRecord);
            if objRecord.length > 0 then
              processRecord;
          until objEof;

          closeIn;
          end;
          {>>>}
        end;
      until eof (cmdFile^.f);

    closeModules;
    closeoutput;

    endPass2Milestone := getMilestone;
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
    friendly := false;
    logging := false;
    history := FALSE;
    modules := false;
    escape := true;

    newline := true;
    usingHistory := FALSE;

    fileIdString := 'no file open.' ;

    { set up pointers for common area list }
    commonHead := nil;
    prevCommon := nil;

    total := 0;
    numUndefinedSymbols := 0;

    for i := -1 TO 15 DO
      begin
      sectbase[i] := 0;
      userbase[i] := -1;  { set up user bases as not needed }
      end;

    for i := 0 TO maxHash DO
      hashTable[i] := nil;

    clearMilestone (startLinkMilestone);
    clearMilestone (endPass1Milestone);
    clearMilestone (endPass2Milestone);
    clearMilestone (endLinkMilestone);
    clearMilestone (endMapGenMilestone);
    clearMilestone (endHisGenMilestone);
    clearMilestone (endSymGenMilestone);
    clearMilestone (endSpaceAllocMilestone);
    clearMilestone (endXrefGenMilestone);
    clearMilestone (startReadHisMilestone);
    clearMilestone (endReadHisMilestone);
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

  { get cmd line }
  cmdLen := -1;
  P_getcmdline (cmd, cmdLen);

  cmdString = '';
  for i := 1 to cmdLen do
    cmdString := cmdString + cmd[i];
  writeln ('cmdString:', cmdString, ' cmdLen:', cmdLen:0);

  { cmd line switches }
  switchSettingsProcess (cmdString);

  { get cmdFilenameString, cmdFileRootString, cmdFileextString from cmdString and ext }
  getFileStrings (cmdString, '.cmd', cmdFileRootString, cmdFileExtString, cmdFilenameString);
  writeln ('cmdString:', cmdString,
           ' cmdFileRootString:', cmdFileRootString,
           ' cmdFileExtString:', cmdFileExtString,
           ' cmdFileNameString:', cmdFileNameString);

  startLinkMilestone := getMilestone;
  startReadHisMilestone := getMilestone;
  endReadHisMilestone := startReadHisMilestone;

  { open .cmd file }
  NEW (cmdFile);
  cmdFile^.next := NIL;
  if chat OR debug then
    writeln ('File given is ', cmdFilenameString);
  reset (cmdFile^.f, cmdFilenameString);
  writeln ('Linking from ', cmdFilenameString);

  pass1;
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
      if NOT friendly then
        write ('Section ',i:2,' Start ',hex(basepos,6,6),' Length ', hex(sectbase[i],6,6));
      baseaddr[i] := basepos;
      basepos := basepos+sectbase[i];
      if NOT friendly then
        writeln (' Finish  ',hex(basepos,6,6));
      end;
    end;
    {>>>}

  if friendly then
    {<<<  report section usage nicely}
    begin
    writeln;
    if sectbase[8] <> 0 then
      begin
      writeln ('Size of P                 (8)  = ', sectbase[8]:8, ' bytes');
      total := total + sectbase[8];
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
  endSpaceAllocMilestone := getMilestone;

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
    pass2;

  {<<<  histoy}
  if history then
    dumpHistory;
  endHisGenMilestone := getMilestone;
  {>>>}
  {<<<  symbolTable}
  if symout then
    dumpSymbols;
  endSymGenMilestone := getMilestone;
  {>>>}
  {<<<  map}
  if map then
    dumpSymbolMap;
  endMapGenMilestone := getMilestone;
  {>>>}
  {<<<  xref}
  if xref then
    dumpXreferences;
  endXrefGenMilestone := getMilestone;
  {>>>}

  if bell then for i := 1 TO 10 DO
    write (chr(7));
  writeln;
  endLinkMilestone := getMilestone;

  if chat OR debug OR (NOT quiet) then
    {<<<  report timings}
    begin
    writeln ('Link started           ', startLinkMilestone.timeOfDay);

    showMilestone ('Pass 1                 ', endPass1Milestone, startLinkMilestone);

    if startReadHisMilestone.millTime <> endReadHisMilestone.millTime then
      showMilestone ('Reading history file   ', endReadHisMilestone, startReadHisMilestone);

    showMilestone ('Space allocation       ', endSpaceAllocMilestone, endPass1Milestone);
    showMilestone ('Pass 2                 ', endPass2Milestone, endSpaceAllocMilestone);

    if history then
      showMilestone ('.HIS generation        ', endHisGenMilestone, endPass2Milestone);
    if symout then
      showMilestone ('.SYM generation        ', endSymGenMilestone, endHisGenMilestone);
    if map then
      showMilestone ('.MAP generation        ', endMapGenMilestone, endSymGenMilestone);
    if xref then
      showMilestone ('.XRF generation        ', endXrefGenMilestone, endMapGenMilestone);
    showMilestone ('Link ended             ', endLinkMilestone, startLinkMilestone);

    writeln;
    writeln ('total CPU time:- ', (endLinkMilestone.millTime - startLinkMilestone.millTime) / 1000:7:2);
    end;
    {>>>}
  if friendly then
    {<<<  report timigs nicely}
    begin
    date (datestring);
    writeln;
    writeln ('Link started ', startLinkMilestone.timeOfDay, ' ', datestring);
    writeln ('Link ended   ', endLinkMilestone.timeOfDay, ' ', datestring);
    end;
    {>>>}

  if logging then
    closeloggingFile;
end.
{>>>}
