{ ql.pas }
PROGRAM qlink (input,output);
{<<<}
const
  maxStringLen = 132;

  maxHash = 4095;

  esc = 27;
  startbase = %x'400';

  bytes_per_file_rec$ = 1536 ;
  recs_per_file_rec$ = {102} (bytes_per_file_rec$ - 4) DIV 15 ;
{>>>}
{<<<}
type
  byte = 0..255;
  word = -32768..32767;

  string = varying [maxStringLen] of char;
  filename = varying [80] of char;
  objrec = varying [255] of char;

  { symbols }
  symbolName = packed array [1..10] of char;
  symbolPtr = ^symbol;

  referencePtr = ^reference;
  {<<<}
  reference = packed record
    rname : symbolName;
    next_ref : referencePtr;
    end;
  {>>>}

  resolvePtr = ^resolve;
  {<<<}
  resolve = packed record
    res_addr : integer;
    res_offset : integer;
    next_res : resolvePtr;
    end;
  {>>>}
  {<<<}
  symbol = packed record
    sname, mname           : symbolName;
    ssect, saddr, comsize  : integer;
    next_sym, next_com     : symbolPtr;
    sdef,sused             : boolean;
    sflagged,shist         : boolean;
    reflist                : referencePtr;
    reslist                : resolvePtr;
    end;
  {>>>}
  {<<<}
  idrec = packed record
    rlen : word;
    rtype : char;
    modname : symbolName;
    end;
  {>>>}

  block = packed array [0..255] of byte;
  bblock = packed array [0..511] of byte;

  {<<<}
  milestone = record
    mill_time, int_time : integer;
    time_of_day : packed array [1..11] of char;
    end;
  {>>>}

  history_type_t = ($history_obj, $history_symbol, $history_ref);
  {<<<}
  history_t = record
    CASE history_type : history_type_t of
      $history_obj :    (obj_addr : integer;);
      $history_symbol : (symbol_addr : integer;
                         symbol_name : symbolName;);
      $history_ref :    (ref_addr, ref_offset : integer;);
    end;
  {>>>}

  {<<<}
  filed_history_t = record CASE boolean of
    true : (num_recs: integer;
            recs: ARRAY [1 .. recs_per_file_rec$] of history_t);
    false: (dummy: packed array [1 .. bytes_per_file_rec$] of char);
    end;
  {>>>}
  history_file_t = FILE of filed_history_t;

  textRecordPtrType = ^textRecordtype;
  {<<<}
  textRecordtype = record
    f: text;
    next_text: textRecordPtrType;
    end;
  {>>>}
{>>>}
{<<<}
var
  firstfile : boolean;

  fil: string;
  file_id : string;
  fil_len : word;

  f: filename;
  cur_file: filename;
  ext: filename;
  defext: filename;
  cmdroot: filename;
  cmdname: filename;
  full_filename: filename;

  cmdfile: textRecordPtrType;

  logFile: text;
  symfile: FILE of block;
  objfile : FILE of block;
  mod_file: text;
  tgtfile: text;
  sfile: text;
  tobjfile: text;
  btgtfile: FILE of bblock;
  bfile: FILE of bblock;

  termchar: char;
  smax, checksum, pass, opos, bpos: integer;
  o: objrec;

  line_length: integer;
  line_buff: packed array [1..maxStringLen+1] of char; {extra byte to stop bound overflow!}

  prev_com : symbolPtr;
  com_head : symbolPtr;

  modules, debug, download, check, bell, xref, map, bin, out, symout : boolean;
  logging, english, quiet, files, chat, history, use_history, escape : boolean;
  newline, duffer, inpacket : boolean;

  hash_table : array [0..maxHash] of symbolPtr;

  dirend, colonpos : integer;
  codestart, codelen, topesd, blockpt, undefsym, result, pflag, numsymbols: integer;
  userbase, sbase, sectbase, baseaddr: array [-1..15] of integer; {element -1 is the ABS section}

  outaddr: array [0..255] of integer;
  esdarr: array [0..255] of integer;
  esdsymarr: array [0..255] of symbolPtr;
  codearr: array [1..64] of integer;

  start_link, end_pass_1, end_pass_2, end_link: milestone;
  end_map_gen, end_his_gen, end_sym_gen, end_space_alloc, end_xref_gen: milestone;
  start_read_his, end_read_his: milestone;

  mod_id: symbolName;

  oblock: bblock;
  bl, inblock: block;
  empty: boolean;

  sn: symbolName;
  spt: symbolPtr;
  orec: objrec;
  total, basepos, i: integer;

  datestring: packed array [1..11] of CHAR;
{>>>}

  {<<<  utils}
  {<<<}
  procedure showmod;

  begin
    writeln ('in module ''',mod_id,''', from file ''',cur_file,'''....');
    if logging then
      writeln (logFile, 'in module ''',mod_id,''', from file ''',cur_file,'''....');
  end;
  {>>>}

  {<<<}
  function seq (f1, f2: filename):boolean;

  begin
    if f1.length <> f2.length then
      seq := false
    else
      seq := f1=f2;
  end;
  {>>>}
  {<<<}
  function ior (i1, i2: integer):integer;

  begin
    ior := ord (uor (uint(i1), uint(i2)));
  end;
  {>>>}
  {<<<}
  function iand (i1, i2: integer):integer;

  begin
    iand := ord(uand(uint(i1),uint(i2)));
  end;
  {>>>}
  {<<<}
  function ixor (i1, i2: integer):integer;

  begin
    ixor := ord(uxor(uint(i1),uint(i2)));
  end;
  {>>>}

  {<<<}
  function mvl (i: integer):integer;

  begin
    mvl := i*256;
  end;
  {>>>}
  {<<<}
  function mvr (i: integer):integer;

  begin
    if i < 0 then
      mvr := int (uor (%x'800000', uint (mvr (int (uand (%x'7FFFFFFF', uint(i)))))))
    else
      mvr := (i DIV 256) MOD (%x'1000000');
  end;
  {>>>}

  {<<<}
  function hexchr (c: char):integer;

  begin
    if (c >= '0') AND (c <= '9') then
      hexchr := ord(c)-ord('0')
    else if (c >= 'a') AND (c <= 'f') then
      hexchr := ord(c)-ord('a')+10
    else if (c >= 'A') AND (c <= 'F') then
      hexchr := ord(c)-ord('A')+10
    else
      writeln ('Duff char ''',c,'''when hex char expected!');
  end;
  {>>>}
  {<<<}
  function null (c: char):boolean;

  begin
    null := (c=chr(13)) OR (c=chr(10)) OR (c=' ') OR (c=chr(9)) OR (c=chr(0));
  end;
  {>>>}
  {<<<}
  function digit (c: char):boolean;

  begin
    digit := (c >= '0') AND (c <= '9');
  end;
  {>>>}
  {<<<}
  function alpha (c: char):boolean;

  begin
    alpha := ((c>='a') AND (c<='z')) OR ((c>='A') AND (c<='Z'));
  end;
  {>>>}
  {<<<}
  function alphanum (c: char):boolean;

  begin
    alphanum := digit(c) OR alpha(c) OR (c='_');
  end;
  {>>>}
  {<<<}
  procedure forceUp (var s: symbolName);

  var
    i:integer;

  begin
    for i := 1 TO 10 DO
      if (s[i] >= 'a') AND (s[i] <= 'z') then
        s[i] := chr (ord (s[i]) + ord('A') - ord('a'));
  end;
  {>>>}

  {<<<}
  procedure openin (f: filename);

  begin
    cur_file := f;
    reset (objfile, f);
    blockpt := 0;
    read (objfile, inblock);
    empty := false;
  end;
  {>>>}
  {<<<}
  procedure getrec (var o: objrec);
  { .ro files are 256 byte fixed size blocks. Within these blocks, records are
    packed end to end i.e. one record can span a block boundary. Each record
    conisits of a single byte <n> followed by <n> data bytes
  }
  var
    i, l1:integer;

  begin
    o.length := inblock[blockpt];

    if (255-blockpt) > o.length then
      l1 := o.length
    else
      l1:=(255-blockpt);

    for i := 1 TO l1 DO
      o[i] := chr(inblock[blockpt+i]);

    blockpt := blockpt+l1+1;
    if (blockpt > 255) then
      begin
      if eof (objfile) then
        empty := true
      else
        read (objfile,inblock);

      blockpt := 0;
      l1 := l1 + 1; {step to start of next xfer}
      for i := l1 TO o.length DO
        o[i] := chr (inblock[i - l1]);
      blockpt := 1 + o.length - l1;
      end;
  end;
  {>>>}
  {<<<}
  procedure opentextin (f: filename);

  begin
    cur_file := f;
    reset (tobjfile, f);
  end;
  {>>>}
  {<<<}
  procedure gettextrec (var o: objrec);
  { .rx files are a text version of the .ro file. Each record is a single line
    of text, written out as hex characters i.e. 2 characters per byte. The record
    length is derived from the bytes on the line. This format is provided to allow
    .rx files to be easily ported from other systems e.g. Unix
  }
  var
    buff: varying [255] of char ;
    bytes, i: integer ;

  begin
    readln (tobjfile, buff) ;
    bytes := buff.length DIV 2 ;
    o.length := bytes ;
    for i := 1 TO bytes DO
      o[i] := chr (hexchr(buff[i*2-1]) * 16 + hexchr(buff[i*2]));
  end;
  {>>>}

  {<<<}
  function hashSym (var s: symbolName):integer;

  var
    h,i:integer;

  begin
    h := 0;
    i := 1;
    while (i < 10) AND (s[i] <> ' ') DO
      begin
      h := h * 97 +ord(s[i]);
      i := i + 1;
      end;

    hashSym := h MOD maxHash;
  end;
  {>>>}
  {<<<}
  function findInsert (var s: symbolName; var s_ptr: symbolPtr; ins: boolean): boolean;
  var
    found : boolean;
    hash : integer;

  begin
    forceUp (s);
    hash := hashSym (s);
    s_ptr := hash_table[hash];

    found := false;
    while (NOT found) AND (s_ptr <> nil) DO
      begin
      if s_ptr^.sname = s then
        found := true
      else
        s_ptr := s_ptr^.next_sym;
      end;

    findInsert := found;

    if (NOT found) AND ins then
      begin
      numsymbols := numsymbols + 1;
      new (s_ptr);
      s_ptr^.next_sym := hash_table[hash];
      hash_table[hash] := s_ptr;

      s_ptr^.sdef := false;
      s_ptr^.sused := true;
      s_ptr^.sflagged := false;
      s_ptr^.sname := s;
      s_ptr^.mname := mod_id;
      s_ptr^.comsize := -1;
      s_ptr^.reflist := nil;
      s_ptr^.reslist := nil;
      end;

  end;
  {>>>}

  {<<<}
  procedure allocCom;

  var
    s_ptr : symbolPtr;

  begin
    s_ptr := com_head;
    while s_ptr <> nil DO
      begin
      s_ptr^.saddr := sectbase[s_ptr^.ssect];
      sectbase[s_ptr^.ssect] := sectbase[s_ptr^.ssect]+s_ptr^.comsize;

      if odd(sectbase[s_ptr^.ssect]) then
        sectbase[s_ptr^.ssect] := sectbase[s_ptr^.ssect]+1;

      s_ptr := s_ptr^.next_com;
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
    res_ptr^.next_res := s^.reslist;
    res_ptr^.res_addr := addr;
    res_ptr^.res_offset := offset;
    s^.reslist := res_ptr;
  end;
  {>>>}
  {<<<}
  procedure dispXref;
  { dump cross references to file from list held per symbol }

  var
    i,refcount: integer;
    s_ptr: symbolPtr;
    ref_file: text;
    r: referencePtr;

  begin
    rewrite (ref_file, cmdroot + '.xrf');

    for i := 0 TO maxHash DO
      if hash_table[i] <> nil then
        begin
        if debug then
          writeln(ref_file,i,':');
        s_ptr := hash_table[i];
        REPEAT
          refcount:=0;
          write (ref_file,s_ptr^.sname,' ',
                 hex (s_ptr^.saddr + baseaddr[s_ptr^.ssect], 6, 6),' ',s_ptr^.mname);
          if s_ptr^.comsize<>-1 then
            writeln (ref_file,' C:',hex(s_ptr^.comsize,4,4))
          else
            if NOT s_ptr^.sdef then
              writeln (ref_file,' Undef!')
            else
              writeln (ref_file);

          if s_ptr^.reflist<>nil then
            begin
            r := s_ptr^.reflist;
            REPEAT
              write (ref_file,r^.rname,'    ');
              refcount := refcount + 1;
              if refcount MOD 6 = 0 then
                writeln (ref_file);
              r := r^.next_ref;
              until r = nil;
            if refcount MOD 6 <> 0 then
              writeln (ref_file);
          end
        else
          writeln(ref_file,'Not referenced ANYWHERE!');

        writeln
          (ref_file,'--------------------------------------------------------------------------');
        s_ptr:=s_ptr^.next_sym;
        until s_ptr=nil;
      end;

    close (ref_file);
  end;
  {>>>}
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
  procedure checkUndef;

  var
    i:integer;
    s_ptr: symbolPtr;

  begin
    writeln ('Undefined symbols:-');
    if logging then
      writeln (logFile, 'Undefined symbols:-');

    for i := 0 TO maxHash DO
      if hash_table[i] <> nil then
        begin
        s_ptr := hash_table[i];

        repeat
          if NOT s_ptr^.sdef then
            begin
            writeln ('''',s_ptr^.sname,''' first referenced in module ''', s_ptr^.mname,'''');
            if logging then
              writeln (logFile, '''',s_ptr^.sname,''' first referenced in module ''', s_ptr^.mname,'''');
            s_ptr^.ssect := -1;
            s_ptr^.saddr := %X'FAFF';
            end;

          s_ptr := s_ptr^.next_sym;
          until s_ptr = nil;
        end;
  end;
  {>>>}
  {<<<}
  procedure doubleDef (s: symbolPtr);

  begin
    showmod;

    writeln ('Doubly defined label  ''',s^.sname,'''');
    writeln ('Previously defined in module ''',s^.mname,'''');

    if logging then
      begin
      writeln (logFile, 'Doubly defined label  ''',s^.sname,'''');
      writeln (logFile, 'Previously defined in module ''',s^.mname,'''');
      end;

    s^.sflagged := true;
  end;
  {>>>}

  {<<<  file utils}
  {<<<}
  procedure readHistory (f: filename);
  { disp history and read_history must have match in file format }

  var
    spt : symbolPtr;
    r : resolvePtr;

    history_rec : filed_history_t;
    res_file : history_file_t;

  begin
    reset (res_file, f);

    while NOT eof (res_file) DO
      begin
      Read (res_file, history_rec);

      for i := 1 TO history_rec.num_recs DO
        WITH history_rec.recs[ i ] DO CASE history_type of
          $history_obj :
            basepos := obj_addr;

          $history_symbol :
            begin
            if findInsert (symbol_name, spt, true) then;

            spt^.shist := TRUE;
            spt^.mname := 'patched!!!';
            spt^.ssect := -1;
            spt^.sdef := true;
            spt^.saddr := symbol_addr;
            end;

          $history_ref :
            begin
            new (r);
            r^.next_res := spt^.reslist;
            r^.res_addr := ref_addr;
            r^.res_offset := ref_offset;
            spt^.reslist := r;
            end;
          end{WITH CASE};

      end;

    close (res_file);
  end;
  {>>>}
  {<<<}
  procedure dispHistory;
  { disp history and read_history must have match in file format }

  var
    i, rescount:integer;
    s_ptr: symbolPtr;
    r : resolvePtr;

    history_rec : history_t;
    filed_history_rec : filed_history_t;
    res_file : history_file_t;

    {<<<}
    procedure send_to_file( rec: history_t );

    begin
      filed_history_rec.num_recs := filed_history_rec.num_recs + 1;
      filed_history_rec.recs[ filed_history_rec.num_recs ] := rec;
      if filed_history_rec.num_recs = recs_per_file_rec$ then
        begin
        Write (res_file, filed_history_rec);
        filed_history_rec.num_recs := 0;
        end;
    end;
    {>>>}

  begin
    rewrite (res_file, cmdroot + '.his');
    filed_history_rec.num_recs := 0;

    history_rec.history_type := $history_obj;
    history_rec.obj_addr := basepos;

    { Write (res_file, history_rec); }
    send_to_file ( history_rec );

    for i:=0 TO maxHash DO
      if hash_table[i] <> nil then
        begin
        s_ptr := hash_table[i];

        repeat
          begin
          if s_ptr^.comsize = -1 then { dont dump commons in history }
            begin
            history_rec.history_type := $history_symbol;
            history_rec.symbol_addr := s_ptr^.saddr+baseaddr[s_ptr^.ssect];
            history_rec.symbol_name := s_ptr^.sname;
            {history_rec.mod_name := s_ptr^.mname;}
            {Write (res_file, history_rec);}
            send_to_file ( history_rec);

            if s_ptr^.reslist <> nil then
              begin
              r := s_ptr^.reslist;
              repeat
                begin
                history_rec.history_type := $history_ref;
                history_rec.ref_addr := r^.res_addr;
                history_rec.ref_offset := r^.res_offset;
                {Write (res_file, history_rec);}
                send_to_file (history_rec);
                r := r^.next_res;
                end until r = nil;
              end;

            end;

          s_ptr := s_ptr^.next_sym;
          end until s_ptr = nil;

        end;

    { --- Send the last one. }
    if filed_history_rec.num_recs > 0 then
      Write (res_file, filed_history_rec);

    close (res_file);
  end;
  {>>>}
  {<<<}
  procedure dispHash;

  var
    i:integer;
    s_ptr: symbolPtr;
    map_file : text;

  begin
    rewrite (map_file,cmdroot+'.map');

    for i := 0 TO maxHash DO
      if hash_table[i]<>nil then
        begin
        if debug then
          writeln (map_file,i,':');
        s_ptr := hash_table[i];

        repeat
          write (map_file,s_ptr^.sname,' ',
          hex (s_ptr^.saddr+baseaddr[s_ptr^.ssect],6,6),' ',s_ptr^.mname);
          if s_ptr^.comsize<>-1 then
            writeln (map_file,' C:',hex(s_ptr^.comsize,4,4))
          else
            if NOT s_ptr^.sdef then
              writeln (map_file,' Undef!')
            else
              writeln(map_file);
          s_ptr := s_ptr^.next_sym;
          until s_ptr = nil;
      end;

    close (map_file);
  end;
  {>>>}

  {<<<}
  procedure genSymbolFile;

  var
    i:integer;
    s_ptr: symbolPtr;
    sym_tab : file of block;
    bl:block;

    {<<<}
    procedure pbyte(b:byte);

    begin
      bl[bpos] := b;
      bpos := bpos + 1;

      if bpos > 255 then
        begin
        write (sym_tab,bl);
        bpos := 0;
        end;
    end;
    {>>>}
    {<<<}
    procedure outdata(s:string);

    var
      i:integer;

    begin
      pbyte (s.length);
      for i := 1 TO s.length DO
        pbyte (ord(s[i]));
    end;
    {>>>}
    {<<<}
    function binint (val: integer): string;

    var
      b:string;
      i:integer;

    begin
      b.length:=4;
      for i:=4 DOWNTO 1 DO
        begin
        b[i] := chr (val);
        val := mvr (val);
        end;

      binint := b;
    end;
    {>>>}
    {<<<}
    procedure out_sym (var s:symbol);

    begin
      WITH s DO
        begin
        outdata ('1' + mname + 'VRLvvvvuuccccccccffffffffxxtttddd');
        outdata ('2' + chr(%x'50') + sname + binint (saddr+baseaddr[ssect]));
        end;
    end;
    {>>>}

  begin
    bpos := 0;

    rewrite (sym_tab, cmdroot + '.sym');

    for i := 0 TO maxHash DO
      if hash_table[i] <> nil then
        begin
        s_ptr := hash_table[i];
        repeat
          out_sym (s_ptr^);
          s_ptr := s_ptr^.next_sym;
          until s_ptr = nil;
        end;

      outdata ('4'+chr(17) + binint(0)); {module end record}
      if bpos>0 then
        begin
        for i := bpos TO 255 DO bl[i]:=0;
        write (sym_tab,bl);
        end;

    close (sym_tab);
  end;
  {>>>}

  {<<<}
  procedure openloggingFile;

  begin
    rewrite (logFile, cmdroot + '.log');
    writeln (logFile, 'Linking from ', full_filename);
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
    writeln (logFile, 'Link started ', start_link.time_of_day, ' ', datestring);
    writeln (logFile, 'Link ended   ', end_link.time_of_day, ' ', datestring);
    writeln (logFile, 'total CPU time:- ',(end_link.mill_time-start_link.mill_time)/1000:7:2);

    close (logFile);
  end;
  {>>>}

  {<<<}
  procedure binByte (b: byte);

  begin
    oblock[opos] := b;
    opos := opos + 1;

    if opos > 511 then
      begin
      if out then
        write (bfile, oblock);
      if download then
        write (btgtfile, oblock);
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
      writeln (tgtfile);
    if out then
      writeln (sfile);
  end;
  {>>>}
  {<<<}
  procedure sendsform (var s: string);

  begin
    if out then
      write (sfile,s);
    if download then
      write (tgtfile,s);
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
  {>>>}

  {<<<}
  function getByte: byte;

  begin
    getByte := ord (o[bpos]);
    bpos := bpos  +1;
  end;
  {>>>}
  {<<<}
  function getSymbolName: symbolName;

  var
    i: integer;
    sn: symbolName;

  begin
    for i := 1 TO 10 DO
      sn[i] := chr(getByte);

    getSymbolName := sn;
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
  procedure procId;

  var
    sect: integer;
    coerce: record
      CASE integer of
        0: (ob: objrec);
        1: (id: idrec);
        end;

  begin
    topesd := 17;
    esdarr[0] := 0;  {unused esd value}

    coerce.ob := o;
    mod_id := coerce.id.modname;

    { we need to init these esd values, in case of zero length sections}
    if pass = 2 then
      begin
      if chat OR debug then
        writeln ('Pass 2 of ', mod_id,':');

      if modules then
        begin
        write (mod_file, mod_id, ':');
        if files then
          begin
          {if file_id.length < 50 then
            write (mod_file, pad(file_id, ' ', 50 ), ':' )
          else
          }
          write (mod_file, file_id, ':' );
          end;
        end;

      for sect := 0 TO 15 DO
        begin
        esdarr[sect+1] := baseaddr[sect]+sbase[sect];
        esdsymarr [topesd] := NIL;
        outaddr[sect+1] := esdarr[sect+1];
        end;
      end

    else if chat OR debug then
      Writeln ('Pass 1 of ', mod_id,':');
  end;
  {>>>}
  {<<<}
  procedure processRec;
  { first pass object record processor }

    {<<<}
    procedure procesd;

      {<<<}
      procedure doEsd;

      var
        ty : byte;
        sect : integer;
        s : symbolName;
        b : boolean;
        i:integer;
        spt : symbolPtr;

        {<<<}
        procedure addRef (s:symbolPtr;mod_name:symbolName);

        var
          r_ptr: referencePtr;

        begin
          new (r_ptr);
          r_ptr^.next_ref := s^.reflist;
          r_ptr^.rname := mod_name;
          s^.reflist := r_ptr;
        end;
        {>>>}

      begin
        ty := getByte;
        sect := ty MOD 16;
        ty := ty DIV 16;

        CASE ty of
          0 : bpos := bpos + 8;
          {<<<}
          1:
            begin { common area symbol }
            s := getSymbolName;
            i := getInt;
            b := findInsert (s,spt,true);

            if debug then
              Writeln ('Common data - section ', sect:2,' ', s, ' length = ', hex(i,6,6));

            if xref then
              addRef (spt,mod_id);

            if NOT spt^.sdef then
              begin
              if b then undefsym:=undefsym-1;
              spt^.mname:=mod_id;
              spt^.ssect:=sect;
              spt^.sdef:=true;
              spt^.comsize:=i;
              if prev_com<>nil then
                prev_com^.next_com:=spt
              else
                com_head:=spt;
              spt^.next_com:=nil;
              prev_com:=spt;
              end

            else
              if (i<>spt^.comsize) then
                begin
                if (NOT spt^.sflagged) AND (spt^.comsize=-1) then
                  begin
                  showmod;
                  writeln('Label ''',s,''' is used double defined - ');
                  writeln('as a common in module ''',mod_id,'''');
                  writeln(' and as an XDEF in module ''',spt^.mname,'''');
                  spt^.sflagged:=true;
                 end

               else if check AND (NOT spt^.sflagged) then
                 begin
                 showmod;
                 writeln('Common area size clash - common ''',s,'''');
                 writeln('size in this module is ',hex(i,6,6),' bytes');
                 writeln('size in ''',spt^.mname,''' is ',hex(spt^.comsize,6,6),
                          ' bytes');
                 spt^.sflagged:=true;
                 end;

               if (i>spt^.comsize) AND (spt^.comsize<>-1) then
                 begin
                 spt^.mname:=mod_id;
                 spt^.comsize:=i;
                 end;
               end;
             end;
          {>>>}
          {<<<}
          2,3 :
            begin { section definition and allocation }
            i := getInt;
            if debug then
              Writeln ('Section - ', sect:2,' ', ' length = ', hex(i,6,6));

            sectbase[sect] := sectbase[sect]+i;
            if odd(sectbase[sect]) then
              sectbase[sect] := sectbase[sect] + 1;
            end;
          {>>>}
          {<<<}
          4,5 :
            begin { symbol defintion }
            if ty = 5 then sect := -1;
            s := getSymbolName;
            b := findInsert (s,spt,true);

            { this isnt right yet, should fix it }
            if (spt^.sdef) AND (NOT spt^.sflagged) then
              begin
              if spt^.shist then { previously defined by history file }
                begin
                if chat then
                  Writeln ('redefining ',s);
                end
              else
                doubledef(spt)
              end

            else
              if b then
                begin
                if spt^.shist then { previously defined by history file }
                  begin
                  if chat then
                    Writeln ('redefining ',s);
                  end
                else
                  undefsym := undefsym - 1;
                end;

            spt^.mname := mod_id;
            spt^.ssect := sect;
            spt^.sdef := true;
            spt^.saddr := getInt + sectbase[sect];
            end;
          {>>>}
          {<<<}
          6,7 :
            begin { symbol reference }
            if ty = 6 then
              begin
              showmod;
              writeln ('xref ',sect);
              end;

            s := getSymbolName;
            b := findInsert (s,spt,true);
            if xref then
              addRef (spt,mod_id);
            if (NOT b) then
              undefsym := undefsym + 1;
            end;
          {>>>}
          {<<<}
          8,9 :
            begin
            showmod;
            writeln ('cl address');
            bpos := bpos + 5;
            end;
          {>>>}
          {<<<}
          10 :
            begin
            showmod;
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
  {<<<}
  procedure procRec2;
  { second pass object record processor }

    {<<<}
    procedure outputData;

    var
      c,pos : integer;

      {<<<}
      procedure sformat(pos,len:integer);

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
            b := int(uand(%x'FFFF',uint(codearr[i+pos]))) DIV 256;
            if (b = esc) AND (escape = true) then
              begin
              oblock[opos] := b;
              opos := opos + 1;
              if opos > 511 then
                begin
                if out then write
                  (bfile,oblock);
                if download then
                  write(btgtfile,oblock);
                opos := 0;
                end;
              end;

            oblock[opos] := b;
            opos := opos + 1;
            if opos > 511 then
              begin
              if out then
                write (bfile,oblock);
              if download then
                write (btgtfile,oblock);
              opos := 0;
              end;

            checksum := ord(uxor(uint(b),uint(checksum)));

            b := (codearr[i+pos] MOD 256);
            if (b = esc) AND (escape = true) then
              begin
              oblock[opos] := b;
              opos := opos + 1;
              if opos > 511 then
                begin
                if out then
                  write(bfile,oblock);
                if download then
                  write(btgtfile,oblock);
                opos := 0;
                end;
              end;

            oblock[opos] := b;
            opos := opos + 1;
            if opos > 511 then
              begin
              if out then
                write(bfile,oblock);
              if download then
                write(btgtfile,oblock);
              opos := 0;
              end;
            checksum := ord (uxor (uint(b), uint (checksum)));
            end
        else
          for i := 1 TO len DO
            begin
            wbyte (mvr(codearr[i+pos]));
            wbyte (codearr[i+pos] MOD 256);
            end;

        endPacket;
      end;
      {>>>}

    begin
      c := codelen;
      pos := 0;
      while c > smax DO
        begin
        sformat (pos,smax);
        pos := pos+smax;
        c := c - smax;
        end;

      if c > 0 then
        sformat(pos,c);
    end;
    {>>>}
    {<<<}
    procedure procesd;

      {<<<}
      procedure doesd;

      var
        sect,ty : byte;
        s : symbolName;
        b : boolean;
        patch, i:integer;
        spt : symbolPtr;
        r : resolvePtr;

      begin
        ty := getByte;
        sect := ty MOD 16;
        ty := ty DIV 16;

        CASE ty of
          {<<<}
          0:
            begin { no idea !! }
            bpos := bpos + 4;

            i := getInt;
            esdarr[topesd] := i;
            esdsymarr [topesd] := NIL;

            outaddr[topesd] := esdarr[topesd];
            topesd := topesd + 1;
            end;
          {>>>}
          {<<<}
          1:
            begin { common area symbol }
            s := getSymbolName;
            bpos := bpos + 4; {skip int}

            b := findInsert (s,spt,false);
            if NOT b then
              begin
              showmod;
              writeln ('internal consistency check failure - lost symbol');
              end;

            esdarr[topesd] := spt^.saddr + baseaddr[spt^.ssect];
            esdsymarr [topesd] := spt;

            outaddr[topesd] := esdarr[topesd];
            topesd := topesd + 1;
            end;
          {>>>}
          {<<<}
          2,3 :
            begin { section symbol }
            i := getInt;
            esdarr[sect+1] := baseaddr[sect] + sbase[sect];
            esdsymarr [topesd] := NIL;

            outaddr[sect+1] := esdarr[sect+1];
            if modules then
              write (mod_file,' ',sect:2,':', hex (esdarr[sect+1],6,6),'+', hex(i,6,6));

            sbase[sect] := sbase[sect] + i;

            if odd (sbase[sect]) then
              sbase[sect] := sbase[sect] + 1;
            end;
          {>>>}
          {<<<}
          4,5 :
            if use_history then
              begin { symbol defintion, use to make patches on second pass }
              s := getSymbolName;
              b := findInsert (s,spt,FALSE); { find it }

              if spt^.reslist <> nil then
                begin
                r := spt^.reslist;
                repeat
                  begin
                  patch := spt^.saddr + baseaddr[spt^.ssect] + r^.res_offset;
                  if debug then writeln ('patching ',hex(r^.res_addr,6,6), ' with ',
                                         hex(patch-r^.res_offset,6,6), ' + ', hex(r^.res_offset,6,6));

                  codestart := r^.res_addr;
                  codearr [1] := mvr (mvr (patch));
                  codearr [2] := patch;
                  codelen := 2;
                  outputData;

                  r := r^.next_res;
                  end until r=nil;
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

            b := findInsert (s,spt,false);
            if NOT b then
              begin
              showmod;
              writeln ('internal check failure - lost symbol');
              end;

            esdarr[topesd] := spt^.saddr + baseaddr[spt^.ssect];
            esdsymarr [topesd] := spt;

            outaddr[topesd] := esdarr[topesd];
            topesd := topesd + 1;
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
      bitmap,curresd:integer;

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
          codearr[codelen] := w;
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
            showmod;
            writeln('Warning - possible assembler foul-up');
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
            if thisesd > topesd then
              begin
              showmod;
              writeln(' assembler foul-up.. trying to use an undefined ESD : '
                      ,thisesd);
              end;

            if odd(i) then
              add := add + esdarr[thisesd]
            else
              add := add - esdarr[thisesd];
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
              showmod;
              writeln ('odd fix-up offset - assembler error .',offset,curresd);
              writeln ('>>',hex(codestart,6,6));
              offset := offset + 1;
              end;

            if codelen > 0 then
              outputData;

            outaddr[curresd] := outaddr[curresd] + codelen*2 + offset;
            codelen := 0;
            codestart := outaddr[curresd];
            end

          else  { numesd <> 0 }
            begin
            if NOT longwd then
              begin
              if (add > 32767) OR (add < -32768) then
                begin
                showmod;
                writeln('Long address generated into word location :',hex(add,8,8));
                end;
              end;

            if esdsymarr [thisesd] <> NIL then { only need named symbols }
              if mod_id <> esdsymarr [thisesd]^.mname then { outside module }
                begin
                if history then
                  { address to be resolved LONGWORD only at present}
                  addRes (esdsymarr [thisesd], codestart + codelen*2, offset);

                if debug then
                  Writeln ('sym ', longwd,
                           ' ', thisesd:2,
                           ' ', esdsymarr [thisesd]^.sname,
                           ' ', hex (add,8,8), ' = ', hex (esdarr[thisesd]),
                           ' + ', hex (offset,4,4), ';', hex (offsize, 1, 1),
                           ' at ', hex (codestart + codelen * 2, 8, 8));
                end;

            { generate resolved address }
            if longwd then adddata (mvr (mvr (add)));
            adddata(add);
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
      codestart := outaddr[curresd];

      while bpos < o.length DO
        procbyte;

      outputData;
      { dont forget convert to bytes}
      outaddr[curresd] := outaddr[curresd]+(codelen*2);
    end;
    {>>>}
    {<<<}
    procedure proceom;

    begin
      if modules then
        writeln(mod_file);
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

  {<<<}
  procedure openOutput;

  begin
    opos := 0;
    inpacket := false;

    if bin then
      begin
      if download then
        begin
        rewrite (btgtfile, 'target.txt');
        writeln ('Downloading binary file - target.txt');
        end;

      if out then
        begin
        rewrite (bfile, cmdroot+'.BIN');
        if chat OR debug OR (NOT quiet) then
          writeln ('Making binary file ', cmdroot + '.BIN');
        if logging then
          writeln (logFile, 'Making binary file ', cmdroot + '.BIN');
        end;
      end

    else
      begin
      if download then
        begin
        rewrite (tgtfile, 'target.txt');
        writeln ('Downloading SR file - target.txt');
        end;

      if out then
        begin
        rewrite (sfile, cmdroot+'.SR');
        writeln ('Making SR file ', cmdroot + '.SR');
        end;
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
        close (btgtfile);
      if out then
        close (bfile);
      end
    else
      begin
      if download then
        close (tgtfile);
      if out then
        close (sfile);
      end;
  end;
  {>>>}

  {<<<}
  procedure switchProcess (var s: string);

    {<<<}
    procedure doSwitch (start, switchLen: integer);
    { processes a switch setting directive }

    var
      noflag : boolean;
      sw : packed array [1..3] of char;
      endpos, sw_end, sect, i, swpos : integer;
      c : char;

      {<<<}
      procedure setSwitch (var b: boolean);

      BEGIN
        b := NOT noflag;
      end;
      {>>>}
      {<<<}
      function gnext: char;

      begin
        gnext := line_buff [swpos];
        if swpos < switchLen then
          swpos := swpos + 1;
      end;
      {>>>}

    begin
      { convert to lowerCase }
      for i := 1 TO switchLen DO
        if (line_buff[i]>='A') AND (line_buff[i]<='Z') then
          line_buff[i] := chr(ord(line_buff[i]) + ord('a') - ord('A'));

      swpos := start;
      repeat
        noflag := false;
        sw[1] := gnext;
        sw[2] := gnext;
        sw[3] := '.';
        if sw = 'no.' then
          {<<<  no.}
          begin
          noflag := true;
          sw[1] := gnext;
          sw[2] := gnext;
          end;
          {>>>}

        sw[3] := gnext;
        sw_end := swpos;
        repeat
          c := gnext;
          until (c='/') OR (null (c)) OR (swpos >= switchLen); {skip to next switch}

        if (sw[1] = 'o') AND (sw[2] >= '0') AND (sw[2] <= '9') then
          {<<<  section startAddress}
          begin
          if sw[3] = ':' then
            sect := ord (sw[2]) - ord('0')
          else
            begin
            sect := 10 * (ord(sw[2]) - ord('0')) + (ord(sw[3]) - ord('0'));
            sw_end := sw_end+1;
            end;

          if swpos <> switchLen then
            endpos := swpos - 2
          else
            endpos := swpos;

          if (sect >= 0) AND (sect <= 15) then
            begin
            userbase[sect] := 0;
            for i := sw_end TO endpos DO
              userbase[sect] := 16 * userbase[sect] + hexchr (line_buff[i]);
            if debug then
              writeln (hex (userbase[sect],6,6),' ',sect);
            end
          else
            writeln (' Illegal section number in switch ', sw);
          end
          {>>>}
        else if (sw = 'xre') OR (sw = 'xrf') then
          setSwitch (xref) { generate xref file }
        else if sw = 'map' then
          setSwitch (map)  { generate map file}
        else if sw = 'sym' then
          setSwitch (symout) { generate symbol file}
        else if sw = 'bin' then
          setSwitch (bin)  { binary output}
        else if sw = 'mod' then
          setSwitch (modules) { module list}
        else if sw = 'deb' then
          setSwitch (debug) { debug mode}
        else if (sw = 'dld') OR (sw='dow') then
          setSwitch (download) {download to target}
        else if sw = 'out' then
          setSwitch (out) { generate any output at all!}
        else if sw = 'cha' then
          setSwitch (chat) { generate loads of output }
        else if sw = 'qui' then
          setSwitch (quiet) { generate minimum output}
        else if sw = 'eng' then
          setSwitch (english) { say understandable things}
        else if sw = 'log' then
          {<<<  log stuff in .log file}
          begin
          setSwitch (logging);
          openloggingFile;
          end
          {>>>}
        else if sw = 'fil' then
          setSwitch (files) { generate put filenames in mod file }
        else if sw = 'his' then
          setSwitch (history) { generate history file }
        else if sw = 'bel' then
          {<<<  bells}
          begin
          { generate bells at end of link }
          setSwitch (bell);
          if noflag then
            writeln ('What do you want, a prize or something??');
          end
          {>>>}
        else if sw = 'che' then
          setSwitch (check) { check all possible grubbies}
        else if sw = 'esc' then
          setSwitch (escape) { replace all 1B's in code with 1B1B }
        else
          writeln ('Unknown switch :', sw );
      until (swpos >= switchLen) OR (null (c));
    end;
    {>>>}

  var
    i, j, l: integer;
    slashfound: boolean;

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
        line_buff[j-i] := s[j]; {pick up everything after the slash }

      doSwitch (1, l-i);
      if i = 1 then
        s := ''
      else
        Writeln ('missing substr');
        {s := substr (s, 1, i-1);
        }
      end;
  end;
  {>>>}

  {<<<}
  function getFile (var terminator: char): filename;

  var
    f : filename;
    c : char;
    s : string;
    tempfile: textRecordPtrType;

  begin
    f := '';
    repeat
      { Get the next char that is part of a filename. }
      repeat
        { Read the next char. }
        read (cmdfile^.f,c);
        if newline then
          {<<<  if it's at the start of a line, handle switches and comments}
          begin
          newline := false;

          { Read sequence of comment/control lines }
          while ((c='!') OR (c='/') OR (c = '@')) AND NOT eof (cmdfile^.f) DO
            {<<<  its a comment/control line}
            begin
            readln (cmdfile^.f,s);

            if c = '@' then
              begin
              {writeln( 'Line read is ', s );}
              while NOT (s[ s.length ] IN [ 'A' .. 'Z', 'a' .. 'z', '0' .. '9', '-', '_' ]) DO
                s.length := s.length - 1;

              writeln( 'File read is ', s );

              NEW (tempfile);
              tempfile^.next_text := cmdfile;
              cmdfile := tempfile;
              reset (cmdfile^.f, s);
              writeln ('File opened is ', s);
              end

            else if (c='/') AND (pass=1) then
              begin
              s := '/'+s;
              switchprocess (s);
              end;

            if NOT eof (cmdfile^.f) then
              read (cmdfile^.f,c);
            end;
            {>>>}

          if eof (cmdfile^.f) then
            if cmdfile^.next_text <> NIL then
              BEGIN
              close (cmdfile^.f);
              cmdfile := cmdfile^.next_text;
              end;
          end;
          {>>>}
        if eoln (cmdfile^.f) then
           {<<<  eoln, eat empty lines}
           begin
           readln (cmdfile^.f);
           while eoln (cmdfile^.f) AND NOT eof (cmdfile^.f) DO
             readln (cmdfile^.f);
           newline := true;
           end;
           {>>>}
      until (NOT null(c)) OR eof (cmdfile^.f);

      if eof (cmdfile^.f) then
        if cmdfile^.next_text <> NIL then
          begin
          close (cmdfile^.f);
          cmdfile := cmdfile^.next_text;
          end;

      if (c <> '=') AND (c <> ',') AND (c <> '!') AND NOT null(c) then
        begin
        f.length := f.length+1;
        f[f.length] := c;
        end;
    until (c = ',') OR (c = '=') OR eof (cmdfile^.f);

    if eof (cmdfile^.f) then
      if cmdfile^.next_text <> NIL then
        begin
        close (cmdfile^.f);
        cmdfile := cmdfile^.next_text;
        end;

    getFile := f;
    terminator := c;
    {writeln( 'getFile returning <', f, '>, terminator <', c, '>' );}
  end;
  {>>>}
  {<<<}
  function getExt (var f: filename; dext: filename): filename;

  var
    i:integer;

  begin
    i := f.length;

    { search backwards, to see if first non-alpha char is a '.' (i.e. extension)}
    while (i > 1) AND alphanum(f[i]) DO
      i := i-1;

    if f[i]='.' then
      Writeln ('missing substr')
      {getExt := substr(f,i,f.length+1-i)
      }
    else
      begin
      getExt := dext;
      f := f + dext;
      end;
  end;
  {>>>}

  {<<<}
  function currentTime: milestone;

  var
    ms : milestone;
    temp :integer;

    {<<<}
    function get_num (startch: integer): integer;

    var
      temp1, temp2: integer;

    begin
      temp1 := ORD( ms.time_of_day[ startch ] ) - ORD( '0' );
      temp2 := ORD( ms.time_of_day[ startch + 1 ] ) - ORD( '0' );
      get_num := (temp1 * 10) + temp2;
    end;
    {>>>}

  begin
    {ms.mill_time := clock;
    }
    time (ms.time_of_day);

    { ms.time_of_day is hh:mm:ss.cc }
    {                   12 45 78 AB }
    ms.int_time := 0;
    temp := get_num( 1 ); {hh}
    ms.int_time := temp;
    temp := get_num( 4 ); {mm}
    ms.int_time := ms.int_time * 60 + temp;
    temp := get_num( 7 ); {ss}
    ms.int_time := ms.int_time * 60 + temp;
    temp := get_num( 10 ); {cc}
    ms.int_time := ms.int_time * 100 + temp;

    currentTime := ms;
  end;
  {>>>}
  {<<<}
  procedure showMilestone (s: string; ms1, ms2: milestone);

  var
    temp, cc, ss, mm, hh: integer;
    time_str: string;

  begin
    temp := ms1.int_time - ms2.int_time;

    cc := temp MOD 100;
    temp := temp DIV 100;
    ss := temp MOD 60;

    temp := temp DIV 60;
    mm := temp MOD 60;

    temp := temp DIV 60;
    hh := temp MOD 60;

    write (s);
    write (ms1.time_of_day,' ',(ms1.mill_time-ms2.mill_time)/1000:7:2);
    writev (time_str, hh :2, ':', mm :2, ':', ss :2, '.', cc :2 );

    for temp := 1 TO time_str.length DO
      if time_str[temp] = ' ' then
        time_str[temp] := '0';

    write ( ' ', time_str );
    writeln ('  ', ((ms1.mill_time-ms2.mill_time)*100) / (end_link.mill_time-start_link.mill_time):7:2,'%');
  end;
  {>>>}
  {<<<}
  procedure hashUsage;

  var
    depth, i, hash_used: integer;
    depth_used: array[ 0 .. 10 ] of integer;
    s_ptr: symbolPtr;

  begin
    hash_used := 0;

    for depth := 0 TO 10 DO
      depth_used[ depth ] := 0;

    for i := 0 TO maxHash DO
      if hash_table[i] <> nil then
        begin
        hash_used := hash_used + 1;
        depth := 0;
        s_ptr := hash_table[i];
        repeat
          depth := depth + 1;
          s_ptr := s_ptr^.next_sym;
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

    chat := FALSE;
    quiet := false;
    english := false;
    logging := false;
    newline := true;

    history := FALSE;
    use_history := FALSE;

    modules := false;
    files := false;
    file_id := 'no file open.' ;

    { set up pointers for common area list }
    com_head := nil;
    prev_com := nil;

    escape := true;
    defext := '.ro';

    total := 0;
    undefsym := 0;

    for i := -1 TO 15 DO
      begin
      sectbase[i] := 0;
      userbase[i] := -1;  { set up user bases as not needed }
      end;

    for i := 0 TO maxHash DO
      hash_table[i] := nil;
  end;
  {>>>}

{ main }
{<<<}
begin
  init;

  { setup switch settings}
  fil_len := -1;
  P_getcmdline (fil, fil_len);
  switchprocess (fil);

  cmdname := fil;
  ext := getExt (cmdname,'.cmd');

  cmdroot := cmdname;
  cmdroot.length := cmdname.length - ext.length;

  {
  colonpos := index (cmdroot,':');
  dirend := index (cmdroot,']');
  if colonpos > dirend then
    dirend := colonpos;
  if dirend <> 0 then
    cmdroot := substr (cmdroot,dirend+1,cmdroot.length-dirend);
  }

  start_link := currentTime;

  start_read_his := currentTime;
  end_read_his := start_read_his;

  NEW (cmdfile);
  cmdfile^.next_text := NIL;

  if chat OR debug then
    writeln ('File given is ', cmdname );

  reset (cmdfile^.f, cmdname);
  full_filename := cmdname;
  writeln ('Linking from ', full_filename);

  pass := 1;
  firstfile := true;
  basepos := startbase;

  repeat
    f := getFile (termchar);
    if termchar = '=' then
      begin
      if firstfile then
        cmdroot := f
      else
        writeln('You can''t put an ''='' THERE!');
      end

    else {process the file}
      begin
      ext := getExt (f, defext);

      if ext = '.his' then { history file of previous link }
        begin
        if use_history then
           Writeln ('Can only use one history file, subsequent ones ignored')
        else
          begin
          start_read_his := currentTime;
          readHistory (f);
          end_read_his := currentTime;
          end;
        use_history := TRUE;
        end

      else if ext = '.rx' then { text format .rx file }
        begin
        opentextin (f);
        repeat
          gettextrec (o);
          if o.length > 0 then
            processrec;
        until eof (tobjfile) ;
        close (tobjfile);
        end

      else { normal .ro file }
        begin
        openin (f);
        repeat
          getrec (o);
          if o.length > 0 then processrec;
        until empty;
        close (objfile);
        end;
      end;

    firstfile := false;
    until eof (cmdfile^.f);

  end_pass_1 := currentTime;
  allocCom;

  hashUsage;

  if chat OR debug OR (NOT quiet) then
    begin
    writeln (numsymbols:5,' in symbol table');
    if logging then
      writeln (logFile, numsymbols:5,' in symbol table');
    end;

  baseaddr[-1] := 0;      {set up base of absolute section}

  for i := 0 TO 15 DO
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

  if english then
    {<<<  report section usage}
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
  end_space_alloc := currentTime;

  if undefsym<>0 then
    begin
    writeln('Number of undefined symbols:- ',undefsym);
    if logging then
      writeln (logFile, 'Number of undefined symbols:- ',undefsym);
    checkUndef;
    end;

  if bin then
    smax := 512 {randomly large number!}
  else
    smax := 16; {s-format max line size}

  if modules OR out OR download then
    {<<<  run second pass}
    begin
    pass := 2;
    openoutput;
    reset (cmdfile^.f);
    newline := true;

    if modules then
      rewrite (mod_file, cmdroot+'.MOD');

    for i := 0 TO 15 DO
      sbase[i] := 0;

    repeat
      f := getFile (termchar);
      if termchar <> '=' then
        begin
        ext := getExt (f,defext);
        if ext = '.his' then
          begin
          { do not reread history file }
          end

        else if ext = '.rx' then
          { text format .rx file }
          begin
          opentextin (f);
          repeat
            gettextrec (o);
            if o.length > 0 then procrec2;
          until eof (tobjfile) ;
          close (tobjfile);
          end

        else
          {normal .ro file}
          begin
          openin(f);
          repeat
            getrec(o);
            if o.length > 0 then procrec2;
          until empty;
          close(objfile);
          end;
        end;
      until eof (cmdfile^.f);

    if modules then
      close (mod_file);

    closeoutput;
    end;
    {>>>}
  end_pass_2 := currentTime;

  if history then
    dispHistory;
  end_his_gen := currentTime;

  if symout then
    genSymbolFile;
  end_sym_gen := currentTime;

  if map then
    dispHash;
  end_map_gen := currentTime;

  if xref then
    dispXref;
  end_xref_gen := currentTime;

  if bell then for i := 1 TO 10 DO
    write (chr(7));
  writeln;
  end_link := currentTime;

  if chat OR debug OR (NOT quiet) then
    begin
    writeln ('Link started           ', start_link.time_of_day);

    showMilestone ('Pass 1                 ', end_pass_1,start_link);
    if start_read_his.mill_time <> end_read_his.mill_time then
      showMilestone ('Reading history file   ', end_read_his, start_read_his );
    showMilestone ('Space allocation       ', end_space_alloc,end_pass_1);
    showMilestone ('Pass 2                 ', end_pass_2,end_space_alloc);
    if history then
      showMilestone ('.HIS generation        ', end_his_gen,end_pass_2);
    if symout then
      showMilestone ('.SYM generation        ', end_sym_gen,end_his_gen);
    if map then
      showMilestone ('.MAP generation        ', end_map_gen,end_sym_gen);
    if xref then
      showMilestone ('.XRF generation        ', end_xref_gen,end_map_gen);
    showMilestone ( 'Link ended             ', end_link, start_link);

    writeln;
    writeln ('total CPU time:- ', (end_link.mill_time - start_link.mill_time) / 1000:7:2);
    end;

  if english then
    begin
    date (datestring);
    writeln;
    writeln ('Link started ', start_link.time_of_day, ' ', datestring);
    writeln ('Link ended   ', end_link.time_of_day, ' ', datestring);
    end;

  if logging then
    closeloggingFile;

end.
{>>>}
