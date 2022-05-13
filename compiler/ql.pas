{ ql.pas }
PROGRAM qlink(input,output);
{<<<}
CONST
  vermsg = 'V1.44';
  max_len = 132;
  max_hash = 4095;
  startbase = %x'400';
  esc       = 27;

{ --- size( history_t ) = 15;
{ Therefore size( filed_history_t ) = 545 * 15 + 4 = 8179.
{ ie. Almost filling 8k.
}
{bytes_per_file_rec$ = 8192 ;
{recs_per_file_rec$ = 545 ;
}

{ --- ICC/JMcD say 3 blocks is cluster size. so try that.
{ Therefore size( filed_history_t ) = 102 * 15 + 4 = 1534.
}

bytes_per_file_rec$ = 1536 ;
recs_per_file_rec$ = {102} (bytes_per_file_rec$ - 4) DIV 15 ;
{>>>}
{<<<}
TYPE
  byte = [byte] 0..255;
  word = [word] -32768..32767;

  sym_name = PACKED ARRAY [1..10] OF char;
  sym_ptr  = ^symbol;

  ref_ptr  = ^reference;
  reference= PACKED RECORD
    rname : sym_name;
    next_ref : ref_ptr;
    END;

   resolve_ptr = ^resolve;
   resolve = PACKED RECORD
     res_addr : INTEGER;
     res_offset : integer;
     next_res : resolve_ptr;
     END;

   string = VARYING [max_len] OF char;
   {<<<}
   symbol   = PACKED RECORD
     sname,mname            : sym_name;
     ssect, saddr, comsize  : integer;
     next_sym,next_com      : sym_ptr;
     sdef,sused             : boolean;
     sflagged,shist         : boolean;
     reflist                : ref_ptr;
     reslist                : resolve_ptr;
     END;
   {>>>}
   {<<<}
   idrec = PACKED RECORD
     rlen : word;
     rtype : char;
     modname : sym_name;
     END;
   {>>>}

  filename = VARYING [80] OF char;
  objrec = VARYING [255] OF char;

  block = PACKED ARRAY [0..255] OF byte;
  bblock = PACKED ARRAY [0..511] OF byte;

  milestone = RECORD
    mill_time, int_time : integer;
    time_of_day : PACKED ARRAY [1..11] OF char;
    END;

  history_type_t = ($history_obj, $history_symbol, $history_ref);
  {<<<}
  history_t = RECORD
              CASE history_type : history_type_t OF
                $history_obj :    (obj_addr : INTEGER;);
                $history_symbol : (symbol_addr : INTEGER;
                                   symbol_name : sym_name;);
                $history_ref :    (ref_addr, ref_offset : INTEGER;);
              END;
  {>>>}

  {<<<}
  filed_history_t = RECORD CASE boolean OF
                    true : (num_recs: integer;
                            recs: ARRAY [ 1 .. recs_per_file_rec$ ] OF history_t);
                    false: (dummy: PACKED ARRAY [ 1 .. bytes_per_file_rec$ ] OF char);
                    END;
  {>>>}
  history_file_t = FILE OF filed_history_t;

  text_ptr_t = ^text_rec_t;
  {<<<}
  text_rec_t = RECORD
               f: text;
               next_text: text_ptr_t;
               END;
  {>>>}
{>>>}

{<<<}
VAR
  firstfile : boolean;
  termchar : char;
  smax,checksum,pass,opos,bpos : integer;
  o : objrec;

  line_length:integer;
  line_buff:PACKED ARRAY [1..max_len+1] of char;
              {extra byte to stop bound overflow!}

  prev_com,com_head : sym_ptr;

  modules,debug,download,check,bell,xref,map,bin,out,symout : boolean;
  log, english, quiet, files, chat, history, use_history, escape : boolean;

  newline,duffer,inpacket : boolean;

  hash_table : ARRAY [0..max_hash] OF sym_ptr;
  fil,prompt, file_id : string;
  fil_len : word;
  dirend,colonpos : integer;
  codestart,codelen,topesd,blockpt,undefsym,result,pflag,numsymbols : integer;
  userbase,sbase,sectbase,baseaddr : ARRAY [-1..15] OF integer;
                                     {element -1 is the ABS section}

  outaddr, esdarr : ARRAY [0..255] OF integer;
  esdsymarr : ARRAY [0..255] OF sym_ptr;

  codearr : ARRAY [1..64] OF integer;

  start_link,end_pass_1,end_pass_2,end_link,
  end_map_gen,end_his_gen,end_sym_gen,end_space_alloc,end_xref_gen,
  start_read_his, end_read_his : milestone;

  mod_id : sym_name;

  symfile,objfile : FILE OF block;
  mod_file, tgtfile, sfile, tobjfile :text;
  btgtfile,bfile : FILE of bblock;

  oblock : bblock;
  bl,inblock : block;
  empty : boolean;
  cur_file,ext,defext,cmdroot,cmdname,full_filename : filename;

  sn:sym_name;
  spt : sym_ptr;
  orec: objrec;
  cmdfile : text_ptr_t;
  f: filename;
  total, basepos, i:integer;

  log_file : text;
  datestring: PACKED ARRAY [1..11] OF CHAR;
{>>>}

{<<<  utils}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE showmod;

BEGIN
writeln('in module ''',mod_id,''', from file ''',cur_file,'''....');
IF log THEN writeln (log_file, 'in module ''',mod_id,''', from file ''',cur_file,'''....');
END; { of PROCEDURE showmod }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
FUNCTION seq(f1,f2:filename):boolean;

BEGIN
IF f1.length<>f2.length THEN
  seq:=false
ELSE
  seq:=f1=f2;
END; { of FUNCTION seq }

{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION ior(i1,i2: integer):integer;

BEGIN
ior:=ord(uor(uint(i1),uint(i2)));
END; {of FUNCTION ior }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION iand(i1,i2: integer):integer;

BEGIN
iand:=ord(uand(uint(i1),uint(i2)));
END; { of FUNCTION iand }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION ixor(i1,i2: integer):integer;

BEGIN
ixor:=ord(uxor(uint(i1),uint(i2)));
END; { of FUNCTION ixor }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION mvl(i:integer):integer;

BEGIN
mvl:=i*256;
END; { of FUNCTION mvl }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION mvr(i:integer):integer;

BEGIN
IF i<0 THEN
  mvr:=int(uor(%x'800000',uint(mvr(int(uand(%x'7FFFFFFF',uint(i)))))))
ELSE
  mvr:=(i DIV 256) MOD (%x'1000000');
END; { of FUNCTION mvr }
{>>>}
{<<<}
FUNCTION hexchr(c:char):integer;

BEGIN
IF (c >= '0') AND (c <= '9') THEN hexchr:=ord(c)-ord('0')
ELSE IF (c >= 'a') AND (c <= 'f') THEN hexchr:=ord(c)-ord('a')+10
ELSE IF (c >= 'A') AND (c <= 'F') THEN hexchr:=ord(c)-ord('A')+10
ELSE writeln('Duff char ''',c,'''when hex char expected!');
END; { of PROCEDURE hexchr }
{>>>}

{<<<}
FUNCTION get_nam( fab_ptr: fab_ptr_t ): string;
VAR
full_name, node_str, dev_str, dir_str, name_str, type_str, ver_str: string;
nam_ptr: nam_ptr_t;

  {I think the fields in the NAM block are rather volatile, they
  get reused very soon, particularly if you do writelns in between
  NAM extractions. So do all the extractions, then write the results. }
  {The RMS manual talks about allocating space by your program for use
  as intermediate space when using RMS services, so thats probably whats
  needed. }

  {<<<}
  PROCEDURE do_string( b: byte; l: unsigned; VAR str: string );

  TYPE
    packed_array = PACKED ARRAY [ 1 .. 80 ] OF CHAR;
    string_ptr_t = ^packed_array;

  VAR
    i: integer;
    string_ptr: string_ptr_t;

  BEGIN
  str := '';
  string_ptr := l :: string_ptr_t;
  FOR i := 1 TO b DO
    str := str + string_ptr^[ i ];
  END;
  {>>>}

BEGIN
nam_ptr := fab_ptr^.fab$l_nam :: nam_ptr_t;

do_string( nam_ptr^.nam$b_node, nam_ptr^.nam$l_node, node_str );
do_string( nam_ptr^.nam$b_dev,  nam_ptr^.nam$l_dev, dev_str );
do_string( nam_ptr^.nam$b_dir,  nam_ptr^.nam$l_dir, dir_str );
do_string( nam_ptr^.nam$b_name, nam_ptr^.nam$l_name, name_str );
do_string( nam_ptr^.nam$b_type, nam_ptr^.nam$l_type, type_str );
do_string( nam_ptr^.nam$b_ver,  nam_ptr^.nam$l_ver, ver_str );

writev( full_name, node_str, dev_str, dir_str, name_str, type_str, ver_str );
get_nam := full_name;
END;
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE openin (f:filename);

BEGIN
cur_file:=f;
open(objfile,f,history:=readonly,sharing:=readonly);

file_id := get_nam( pas$fab( objfile ));

reset(objfile);
blockpt:=0;
read(objfile,inblock);
empty:=false;
END; {of PROCEDURE openin }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE getrec(VAR o:objrec);

VAR
  i,l1:integer;

BEGIN
{.ro files are 256 byte fixed size blocks. Within these blocks, records are
packed end to end i.e. one record can span a block boundary. Each record
conisits of a single byte <n> followed by <n> data bytes}

o.length:=inblock[blockpt];

IF (255-blockpt) > o.length THEN l1:=o.length ELSE l1:=(255-blockpt);

FOR i:=1 TO l1 DO o[i]:=chr(inblock[blockpt+i]);
blockpt:=blockpt+l1+1;
IF (blockpt>255) THEN
  BEGIN
  IF eof(objfile) THEN
    empty:=true
  ELSE
    read(objfile,inblock);

  blockpt := 0;
  l1 := l1 + 1; {step to start of next xfer}
  FOR i := l1 TO o.length DO o[i] := chr (inblock[i - l1]);
  blockpt := 1 + o.length - l1;
  END;

END; { of PROCEDURE getrec }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE opentextin (f:filename);

BEGIN
cur_file:=f;
open(tobjfile,f,history:=readonly,sharing:=readonly);
file_id := get_nam( pas$fab( tobjfile ));
reset(tobjfile);
END; {of PROCEDURE opentextin }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE gettextrec(VAR o:objrec);

VAR
  buff: VARYING [255] of char ;
  bytes, i: integer ;

BEGIN
{.rx files are a text version of the .ro file. Each record is a single line
of text, written out as hex characters i.e. 2 characters per byte. The record
length is derived from the bytes on the line. This format is provided to allow
.rx files to be easily ported from other systems e.g. Unix}

readln (tobjfile, buff) ;
bytes := buff.length DIV 2 ;
o.length := bytes ;
FOR i := 1 TO bytes DO
  o[i] := chr (hexchr(buff[i*2-1]) * 16 + hexchr(buff[i*2]));

END; { of PROCEDURE gettextrec }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
FUNCTION null(c:char):boolean;

BEGIN
null := (c=chr(13)) OR (c=chr(10)) OR (c=' ') OR (c=chr(9)) OR (c=chr(0));
END; { of FUNCTION null }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION digit(c:char):boolean;

BEGIN
digit := (c >= '0') AND (c <= '9');
END; { of PROCEDURE digit }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION alpha(c:char):boolean;

BEGIN
alpha := ((c>='a') AND (c<='z')) OR ((c>='A') AND (c<='Z'));
END; { of PROCEDURE alpha }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION alphanum(c:char):boolean;

BEGIN
alphanum := digit(c) OR alpha(c) OR (c='_');
END; { of PROCEDURE alphanum }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE force_up(VAR s:sym_name);

VAR
  i:integer;

BEGIN
FOR i := 1 TO 10 DO
  IF (s[i] >= 'a') AND (s[i] <= 'z') THEN
    s[i] := chr (ord (s[i]) + ord('A') - ord('a'));
END; { of PROCEDURE force_up }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
PROCEDURE init_hash;

VAR
  i:integer;

BEGIN
FOR i := 0 TO max_hash DO hash_table[i] := nil;
END; { of PROCEDURE init_hash }

{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION hash_sym (VAR s:sym_name):integer;

VAR
  h,i:integer;

BEGIN
h := 0;
i := 1;
WHILE (i < 10) AND (s[i] <> ' ') DO
  BEGIN
  h := h * 97 +ord(s[i]);
  i := i + 1;
  END;

hash_sym := h MOD max_hash;
END; { of FUNCTION hash_sym }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION find_insert (VAR s:sym_name; VAR s_ptr:sym_ptr;
                         ins : boolean):boolean;
VAR
  found : boolean;
  hash : integer;

BEGIN
force_up(s);
hash := hash_sym(s);
s_ptr := hash_table[hash];

found := false;
WHILE (NOT found) AND (s_ptr <> nil) DO
  BEGIN
  IF s_ptr^.sname = s THEN
    found := true
  ELSE
    s_ptr := s_ptr^.next_sym;
  END;

find_insert := found;
IF (NOT found) AND ins THEN
  BEGIN
  numsymbols := numsymbols + 1;
  new(s_ptr);
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
  END;

END; { of FUNCTION find_insert }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
PROCEDURE disp_xref;
{--- dump cross references to file from list held per symbol }

VAR
  i,refcount:integer;
  s_ptr: sym_ptr;
  ref_file : text;
  r : ref_ptr;

BEGIN
open(ref_file,cmdroot+'.xrf',history:=new);
rewrite(ref_file);

FOR i:=0 TO max_hash DO
  IF hash_table[i]<>nil THEN
    BEGIN
    IF debug THEN writeln(ref_file,i,':');
    s_ptr:=hash_table[i];
    REPEAT
      refcount:=0;
      write(ref_file,s_ptr^.sname,' ',
            hex(s_ptr^.saddr+baseaddr[s_ptr^.ssect],6,6),' ',s_ptr^.mname);
      IF s_ptr^.comsize<>-1 THEN
        writeln(ref_file,' C:',hex(s_ptr^.comsize,4,4))
      ELSE
        IF NOT s_ptr^.sdef THEN
          writeln(ref_file,' Undef!')
        ELSE
          writeln(ref_file);

      IF s_ptr^.reflist<>nil THEN
        BEGIN
        r:=s_ptr^.reflist;
        REPEAT
          write(ref_file,r^.rname,'    ');
          refcount:=refcount+1;
          IF refcount MOD 6 = 0 THEN writeln(ref_file);
          r:=r^.next_ref;
          UNTIL r=nil;
        IF refcount MOD 6<>0 THEN writeln(ref_file);
      END
    ELSE writeln(ref_file,'Not referenced ANYWHERE!');

    writeln(ref_file
,'--------------------------------------------------------------------------');
    s_ptr:=s_ptr^.next_sym;
    UNTIL s_ptr=nil;
  END;

close(ref_file);
END;  { of PROCEDURE disp_xref }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE add_res (s:sym_ptr; addr, offset : integer);
{--- add a resolved symbol reference to list held per symbol }

VAR
  res_ptr: resolve_ptr;

BEGIN { of PROCEDURE add_res }
new(res_ptr);
res_ptr^.next_res := s^.reslist;
res_ptr^.res_addr := addr;
res_ptr^.res_offset := offset;
s^.reslist := res_ptr;
END; { of PROCEDURE add_res }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
PROCEDURE read_history ( f : filename);
{--- disp history and read_history must have match in file format }

VAR
  spt : sym_ptr;
  r : resolve_ptr;

  history_rec : filed_history_t;
  res_file : history_file_t;

BEGIN
open (res_file, f, history := readonly, sharing := readonly);
reset (res_file);

WHILE NOT eof (res_file) DO
  BEGIN
  Read (res_file, history_rec);

  FOR i := 1 TO history_rec.num_recs DO
    WITH history_rec.recs[ i ] DO CASE history_type OF
      $history_obj :
        basepos := obj_addr;

      $history_symbol :
        BEGIN
        IF find_insert(symbol_name, spt, true) THEN;
        spt^.shist := TRUE;
        spt^.mname := 'patched!!!';
        spt^.ssect := -1;
        spt^.sdef := true;
        spt^.saddr := symbol_addr;
        END;

      $history_ref :
        BEGIN
        new (r);
        r^.next_res := spt^.reslist;
        r^.res_addr := ref_addr;
        r^.res_offset := ref_offset;
        spt^.reslist := r;
        END;
      END{WITH CASE};

  END;

close (res_file);
END;  { of PROCEDURE read_history }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE disp_history;
{--- disp history and read_history must have match in file format }

VAR
  i, rescount:integer;
  s_ptr: sym_ptr;
  r : resolve_ptr;

  history_rec : history_t;
  filed_history_rec : filed_history_t;
  res_file : history_file_t;

  PROCEDURE send_to_file( rec: history_t );
  BEGIN
  filed_history_rec.num_recs := filed_history_rec.num_recs + 1;
  filed_history_rec.recs[ filed_history_rec.num_recs ] := rec;
  IF filed_history_rec.num_recs = recs_per_file_rec$ THEN
    BEGIN
    Write (res_file, filed_history_rec);
    filed_history_rec.num_recs := 0;
    END;
  END;

BEGIN
open(res_file,cmdroot+'.his',history:=new);
rewrite(res_file);
filed_history_rec.num_recs := 0;

history_rec.history_type := $history_obj;
history_rec.obj_addr := basepos;

{Write (res_file, history_rec);}
send_to_file( history_rec );

FOR i:=0 TO max_hash DO IF hash_table[i] <> nil THEN
  BEGIN
  s_ptr := hash_table[i];

  REPEAT
    BEGIN
    IF s_ptr^.comsize = -1 THEN { dont dump commons in history }
      BEGIN
      history_rec.history_type := $history_symbol;
      history_rec.symbol_addr := s_ptr^.saddr+baseaddr[s_ptr^.ssect];
      history_rec.symbol_name := s_ptr^.sname;
      {history_rec.mod_name := s_ptr^.mname;}
      {Write (res_file, history_rec);}
      send_to_file( history_rec);

      IF s_ptr^.reslist <> nil THEN
        BEGIN
        r := s_ptr^.reslist;
        REPEAT
          BEGIN
          history_rec.history_type := $history_ref;
          history_rec.ref_addr := r^.res_addr;
          history_rec.ref_offset := r^.res_offset;
          {Write (res_file, history_rec);}
          send_to_file( history_rec);
          r := r^.next_res;
          END UNTIL r=nil;
        END;

      END;

    s_ptr := s_ptr^.next_sym;
    END UNTIL s_ptr = nil;

  END;

{ --- Send the last one. }
IF filed_history_rec.num_recs > 0 THEN Write (res_file, filed_history_rec);
close(res_file);
END;  { of PROCEDURE disp_history }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE disp_hash;

VAR
  i:integer;
  s_ptr: sym_ptr;
  map_file : text;

BEGIN
open(map_file,cmdroot+'.map',history:=new);
rewrite(map_file);

FOR i:=0 TO max_hash DO
  IF hash_table[i]<>nil THEN
    BEGIN
    IF debug THEN writeln(map_file,i,':');
    s_ptr:=hash_table[i];

    REPEAT
      write(map_file,s_ptr^.sname,' ',
      hex(s_ptr^.saddr+baseaddr[s_ptr^.ssect],6,6),' ',s_ptr^.mname);
      IF s_ptr^.comsize<>-1 THEN
        writeln(map_file,' C:',hex(s_ptr^.comsize,4,4))
      ELSE
        IF NOT s_ptr^.sdef THEN
          writeln(map_file,' Undef!')
        ELSE
          writeln(map_file);
      s_ptr:=s_ptr^.next_sym;
      UNTIL s_ptr=nil;
  END;

close(map_file);
END; { of PROCEDURE disp_hash }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
PROCEDURE gen_sym_file;

VAR
  i:integer;
  s_ptr: sym_ptr;
  sym_tab : file OF block;
  bl:block;


  {<<<}
  PROCEDURE pbyte(b:byte);

  BEGIN
  bl[bpos]:=b;
  bpos:=bpos+1;

  IF bpos>255 THEN
    BEGIN
    write(sym_tab,bl);
    bpos:=0;
    END;
  END; { of PROCEDURE pbyte }
  {>>>}
  {<<<}
  PROCEDURE outdata(s:string);

  VAR
    i:integer;

  BEGIN { of PROCEDURE outdata }
  pbyte(s.length);
  FOR i:=1 TO s.length DO pbyte(ord(s[i]));
  END; { of PROCEDURE outdata }
  {>>>}
  {<<<}
  FUNCTION binint(val:integer):string;

  VAR
    b:string;
    i:integer;

  BEGIN { of FUNCTION bintval }
  b.length:=4;
  FOR i:=4 DOWNTO 1 DO
    BEGIN
    b[i]:=chr(val);
    val:=mvr(val);
    END;

  binint:=b;
  END; { of FUNCTION bintval }
  {>>>}
  {<<<}
  PROCEDURE out_sym(VAR s:symbol);

  BEGIN { of PROCEDURE out_sym }
  WITH s DO
    BEGIN
    outdata('1'+mname+'VRLvvvvuuccccccccffffffffxxtttddd');
    outdata('2'+chr(%x'50')+sname+binint(saddr+baseaddr[ssect]));
    END;
  END; { of PROCEDURE out_sym }
  {>>>}

BEGIN
bpos := 0;

open(sym_tab,cmdroot+'.sym',history:=new);
rewrite(sym_tab);

FOR i:=0 TO max_hash DO
  IF hash_table[i]<>nil THEN
    BEGIN
    s_ptr:=hash_table[i];
    REPEAT
      out_sym(s_ptr^);
      s_ptr:=s_ptr^.next_sym;
      UNTIL s_ptr=nil;
    END;

  outdata('4'+chr(17)+binint(0)); {module end record}
  IF bpos>0 THEN
    BEGIN
    FOR i:= bpos TO 255 DO bl[i]:=0;
    write(sym_tab,bl);
    END;

close(sym_tab);
END; { of PROCEDURE gen_sym_file }
{>>>}
{>>>}
{<<<}
PROCEDURE open_log_file;

BEGIN
  open (log_file,cmdroot+'.log',history:=new);
  rewrite (log_file);
  writeln (log_file, 'Linking from ', full_filename);
END; 
{>>>}
{<<<}
PROCEDURE close_log_file;

VAR
  total, i:integer;
  datestring: PACKED ARRAY [1..11] OF CHAR;

BEGIN
IF english THEN
  BEGIN
  writeln (log_file);
  total := 0;
  IF sectbase[8] <> 0 THEN
    BEGIN
    writeln (log_file, 'Size of P                 (8)  = ', sectbase[8]:8, ' bytes');
    total := total + sectbase[8];
    END;
  IF sectbase[9] <> 0 THEN
    BEGIN
    writeln (log_file, 'Size of HELP              (9)  = ',
              sectbase[9]:8, ' bytes'); total := total + sectbase[9];
    END;
  IF sectbase[12] <> 0 THEN
    BEGIN
    writeln (log_file, 'Size of error messages   (12)  = ', sectbase[12]:8, ' bytes');
    total := total + sectbase[12];
    END;
  IF sectbase[13] <> 0 THEN
    BEGIN
    writeln (log_file, 'Size of code & constants (13)  = ', sectbase[13]:8, ' bytes');
    total := total + sectbase[13];
    END;
  IF sectbase[14] <> 0 THEN
    BEGIN
    writeln (log_file, 'Size of diagnostic block (14)  = ', sectbase[14]:8, ' bytes');
    total := total + sectbase[14];
    END;
  IF sectbase[15] <> 0 THEN
    BEGIN
    writeln (log_file, 'Size of global variables (15)  = ', sectbase[15]:8, ' bytes');
    total := total + sectbase[15];
    END;
  writeln (log_file, 'Total size                     = ', total:8, ' bytes');
  END

ELSE FOR i:=0 TO 15 DO
  IF sectbase[i] <> 0 THEN
    BEGIN
    write(log_file, 'Section ',i:2,' Start ',hex(baseaddr[i],6,6),' Length ', hex(sectbase[i],6,6));
    writeln(log_file, ' Finish  ',hex(baseaddr[i]+sectbase[i],6,6));
    END;

date (datestring);
writeln (log_file);
writeln (log_file, 'Link started ', start_link.time_of_day, ' ', datestring);
writeln (log_file, 'Link ended   ', end_link.time_of_day, ' ', datestring);
writeln(log_file, 'total CPU time:- ',(end_link.mill_time-start_link.mill_time)/1000:7:2);

close(log_file);
END; 
{>>>}
{<<<}
PROCEDURE alloc_com;

VAR
  s_ptr : sym_ptr;

BEGIN
  s_ptr := com_head;
  WHILE s_ptr <> nil DO
    BEGIN
    s_ptr^.saddr := sectbase[s_ptr^.ssect];
    sectbase[s_ptr^.ssect] := sectbase[s_ptr^.ssect]+s_ptr^.comsize;

    IF odd(sectbase[s_ptr^.ssect]) THEN
      sectbase[s_ptr^.ssect] := sectbase[s_ptr^.ssect]+1;

    s_ptr := s_ptr^.next_com;
    END;
END; 
{>>>}

{<<<}
PROCEDURE overlap_check;

VAR
  i,j:integer;


  FUNCTION clash(i,j:integer):boolean;

  BEGIN
  clash := NOT (((sectbase[i]+baseaddr[i])<=baseaddr[j]) OR
                ((sectbase[j]+baseaddr[j])<=baseaddr[i]));
  END; 

BEGIN 
  FOR i := 0 TO 14 DO
    FOR j := i+1 TO 15 DO
      IF clash(i,j) THEN
        BEGIN
        writeln('Sections ',i:2,' and ',j:2,' overlap!');
        END;
END;
{>>>}
{<<<}
PROCEDURE check_undef;

VAR
  i:integer;
  s_ptr: sym_ptr;

BEGIN
  writeln ('Undefined symbols:-');
  IF log THEN 
    writeln (log_file, 'Undefined symbols:-');

  FOR i := 0 TO max_hash DO
    IF hash_table[i] <> nil THEN
      BEGIN
      s_ptr := hash_table[i];

      REPEAT
        IF NOT s_ptr^.sdef THEN
          BEGIN
          writeln ('''',s_ptr^.sname,''' first referenced in module ''', s_ptr^.mname,'''');
          IF log THEN
            writeln (log_file, '''',s_ptr^.sname,''' first referenced in module ''', s_ptr^.mname,''''); 
          s_ptr^.ssect := -1;
          s_ptr^.saddr := %X'FAFF';
          END;

        s_ptr := s_ptr^.next_sym;
        UNTIL s_ptr=nil;
      END;
END; 
{>>>}
{<<<}
FUNCTION gbyte : byte;

BEGIN
  gbyte := ord(o[bpos]);
  bpos := bpos+1;
END;
{>>>}
{<<<}
FUNCTION getnam : sym_name;

VAR
  i:integer;
  sn : sym_name;

BEGIN
  FOR i := 1 TO 10 DO 
    sn[i] := chr(gbyte);
  getnam := sn;
END; 
{>>>}
{<<<}
FUNCTION getint : integer;

VAR
  i,j:integer;

BEGIN
  i := 0;
  FOR j := 1 TO 4 DO
    i := mvl(i) + gbyte;
  getint := i;
END;
{>>>}
{<<<}
PROCEDURE doubledef (s: sym_ptr);

BEGIN
  showmod;
  writeln('Doubly defined label  ''',s^.sname,'''');
  writeln('Previously defined in module ''',s^.mname,'''');
  IF log THEN
    BEGIN
    writeln(log_file, 'Doubly defined label  ''',s^.sname,'''');
    writeln(log_file, 'Previously defined in module ''',s^.mname,'''');
    END;

  s^.sflagged := true;
END; 
{>>>}
{<<<}
PROCEDURE binbyte (b: byte);

BEGIN
  oblock[opos] := b;
  opos := opos + 1;
  IF opos > 511 THEN
    BEGIN
    IF out THEN 
      write(bfile,oblock);
    IF download THEN 
      write(btgtfile,oblock);
    opos := 0;
    END;

END;
{>>>}

{<<<}
{-------------------------------------------------------------------------}
PROCEDURE sendbin (b: byte);

BEGIN
IF (b = esc) AND (escape = true) THEN binbyte (b);
binbyte (b);
END; { of PROCEDURE sendbin }
{>>>}
{<<<}
PROCEDURE sendsfnewline;

BEGIN
  IF download THEN 
    writeln (tgtfile);
  IF out THEN 
    writeln (sfile);
END;
{>>>}
{<<<}
PROCEDURE sendsform (VAR s: string);

BEGIN
  IF out THEN 
    write(sfile,s);
  IF download THEN 
    write(tgtfile,s);
END;
{>>>}
{<<<}
PROCEDURE wbyte (b: byte);

VAR
  s:string;

BEGIN
  {<<<}
  IF bin THEN
    BEGIN
    sendbin(b);
    checksum := ixor(checksum,b);
    END

  ELSE
    BEGIN
    s := hex(b,2,2);
    sendsform(s);
    checksum := checksum + b;
    END;
  {>>>}

END; { of PROCEDURE wbyte }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE endpacket;

VAR
  s:string;

BEGIN
IF bin THEN
  sendbin(checksum)
ELSE
  BEGIN
  s := hex(255-(checksum MOD 256),2,2);
  sendsform(s);
  sendsfnewline;
  END;
END; { of PROCEDURE endpacket }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
PROCEDURE procid;     {same on both passes}

VAR
  coerce : RECORD
           CASE integer OF
             0:(ob:objrec);
             1:(id:idrec);
             END;
  sect : integer;

BEGIN
topesd := 17;
esdarr[0] := 0;  {unused esd value}

coerce.ob := o;
mod_id := coerce.id.modname;

{--- we need to init these esd values, in case of zero length sections}
IF pass = 2 THEN
  BEGIN
  IF chat OR debug THEN writeln ('Pass 2 of ', mod_id,':');
  IF modules THEN
    BEGIN
    write(mod_file,mod_id,':');
    IF files THEN
      BEGIN
      IF file_id.length < 50 THEN
        write( mod_file, pad( file_id, ' ', 50 ), ':' )
      ELSE
        write( mod_file, file_id, ':' );
      END;
    END;

  FOR sect := 0 TO 15 DO
    BEGIN
    esdarr[sect+1] := baseaddr[sect]+sbase[sect];
    esdsymarr [topesd] := NIL;
    outaddr[sect+1] := esdarr[sect+1];
    END;
  END

ELSE
  IF chat OR debug THEN writeln ('Pass 1 of ', mod_id,':');

END; { of PROCEDURE procid }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE processrec; { first pass object record processor }

  {<<<}
  PROCEDURE procesd;

    {<<<}
    PROCEDURE add_ref(s:sym_ptr;mod_name:sym_name);

    VAR
      r_ptr: ref_ptr;

    BEGIN { of PROCEDURE add_ref }
    new(r_ptr);
    r_ptr^.next_ref := s^.reflist;
    r_ptr^.rname := mod_name;
    s^.reflist := r_ptr;
    END; { of PROCEDURE add_ref }
    {>>>}
    {<<<}
    PROCEDURE doesd;

    VAR
      ty : byte;
      sect : integer;
      s : sym_name;
      b : boolean;
      i:integer;
      spt : sym_ptr;

    BEGIN { of PROCEDURE doesd }
    ty := gbyte;
    sect := ty MOD 16;
    ty := ty DIV 16;

    CASE ty OF
      0 : bpos:=bpos+8;

      1: BEGIN { common area symbol }
         s := getnam;
         i := getint;
         b := find_insert(s,spt,true);

         IF debug THEN Writeln ('Common data - section ', sect:2,' ',
                               s, ' length = ', hex(i,6,6));
         IF xref THEN add_ref(spt,mod_id);
         IF NOT spt^.sdef THEN
           BEGIN
           IF b THEN undefsym:=undefsym-1;
           spt^.mname:=mod_id;
           spt^.ssect:=sect;
           spt^.sdef:=true;
           spt^.comsize:=i;
           IF prev_com<>nil THEN
             prev_com^.next_com:=spt
           ELSE
             com_head:=spt;
           spt^.next_com:=nil;
           prev_com:=spt;
           END

         ELSE
           IF (i<>spt^.comsize) THEN
             BEGIN
             IF (NOT spt^.sflagged) AND (spt^.comsize=-1) THEN
               BEGIN
               showmod;
               writeln('Label ''',s,''' is used double defined - ');
               writeln('as a common in module ''',mod_id,'''');
               writeln(' and as an XDEF in module ''',spt^.mname,'''');
               spt^.sflagged:=true;
              END

            ELSE IF check AND (NOT spt^.sflagged) THEN
              BEGIN
              showmod;
              writeln('Common area size clash - common ''',s,'''');
              writeln('size in this module is ',hex(i,6,6),' bytes');
              writeln('size in ''',spt^.mname,''' is ',hex(spt^.comsize,6,6),
                       ' bytes');
              spt^.sflagged:=true;
              END;

            IF (i>spt^.comsize) AND (spt^.comsize<>-1) THEN
              BEGIN
              spt^.mname:=mod_id;
              spt^.comsize:=i;
              END;
            END;
          END;

      2,3 : BEGIN { section definition and allocation }
            i := getint;
            IF debug THEN Writeln ('Section - ', sect:2,' ',
                                  ' length = ', hex(i,6,6));
            sectbase[sect] := sectbase[sect]+i;
            IF odd(sectbase[sect]) THEN sectbase[sect] := sectbase[sect] + 1;
            END;

     4,5 : BEGIN { symbol defintion }
           IF ty = 5 THEN sect := -1;
           s := getnam;
           b := find_insert(s,spt,true);

           {--- this isnt right yet, should fix it }
           IF (spt^.sdef) AND (NOT spt^.sflagged) THEN
             BEGIN
             IF spt^.shist THEN { previously defined by history file }
               BEGIN
               IF chat THEN Writeln ('redefining ',s);
               END
             ELSE
               doubledef(spt)
             END

           ELSE
             IF b THEN
               BEGIN
               IF spt^.shist THEN { previously defined by history file }
                 BEGIN
                 IF chat THEN Writeln ('redefining ',s);
                 END
               ELSE
                 undefsym := undefsym - 1;
               END;

           spt^.mname := mod_id;
           spt^.ssect := sect;
           spt^.sdef := true;
           spt^.saddr := getint + sectbase[sect];
           END;

      6,7 : BEGIN { symbol reference }
            IF ty = 6 THEN
              BEGIN
              showmod;
              writeln('xref ',sect);
              END;

            s := getnam;
            b := find_insert(s,spt,true);
            IF xref THEN add_ref(spt,mod_id);
            IF (NOT b) THEN undefsym := undefsym + 1;
            END;

      8,9 : BEGIN
            showmod;
            writeln('cl address');
            bpos := bpos + 5;
            END;

      10 : BEGIN
           showmod;
           writeln('cl addr common');
           bpos := bpos + 15;
           END;
       END;

    END; { of PROCEDURE doesd }
    {>>>}

  BEGIN { of PROCEDURE procesd }
  bpos := 2;
  WHILE bpos < o.length DO doesd;
  END; { of PROCEDURE procesd }
  {>>>}
  {<<<}
  PROCEDURE proctxt;

  BEGIN
  END;
  {>>>}
  {<<<}
  PROCEDURE proceom;

  BEGIN
  END;
  {>>>}
  {<<<}
  FUNCTION gnam : sym_name;

  VAR
    s:sym_name;
    i:integer;

  BEGIN
  s := '          ';
  i := 1;
  REPEAT
    s[i] := chr(gbyte);
    i := i + 1;
    UNTIL (i > 10) OR (bpos > o.length);

  gnam := s;
  END;
  {>>>}

BEGIN { of PROCEDURE processrec }
CASE o[1] OF
  '1': procid;
  '2': procesd;
  '3': proctxt;
  '4': proceom;
  END;

END; { of PROCEDURE processrec }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE procrec2;   { second pass object record processor }


  PROCEDURE output_data;

  VAR
    c,pos : integer;


    {<<<}
    PROCEDURE sformat(pos,len:integer);

    VAR
      b,cstart,i:integer;

      {<<<}
      PROCEDURE startpacket;

      VAR
        pktstart : string;
        plen : integer;

      BEGIN
      cstart := codestart + pos*2;
      plen := len*2 + 4; {this happens to be right for both}

      IF bin THEN
        BEGIN
        binbyte(esc);
        binbyte(0);
        wbyte(1);
        wbyte(mvr(plen));
        wbyte(plen MOD 256);
        wbyte(mvr(mvr(mvr(cstart))));
        wbyte(mvr(mvr(cstart)));
        wbyte(mvr(cstart));
        wbyte(cstart MOD 256);
        END

      ELSE
        BEGIN
        pktstart:='S2';
        sendsform(pktstart);
        wbyte(plen);
        wbyte(mvr(mvr(cstart)));
        wbyte(mvr(cstart));
        wbyte((cstart MOD 256));
        END;
      END; { of PROCEDURE start packet }
      {>>>}

    BEGIN { of PROCEDURE startformat }
    checksum := 0;
    startpacket;
    IF bin THEN
    FOR i := 1 TO len DO
      BEGIN
      b := int(uand(%x'FFFF',uint(codearr[i+pos]))) DIV 256;
      IF (b = esc) AND (escape = true) THEN
        BEGIN
        oblock[opos] := b;
        opos := opos + 1;
        IF opos > 511 THEN
          BEGIN
          IF out THEN write(bfile,oblock);
          IF download THEN write(btgtfile,oblock);
          opos := 0;
          END;
        END;

      oblock[opos] := b;
      opos := opos + 1;
      IF opos > 511 THEN
        BEGIN
        IF out THEN write(bfile,oblock);
        IF download THEN write(btgtfile,oblock);
        opos := 0;
        END;

      checksum := ord(uxor(uint(b),uint(checksum)));

      b := (codearr[i+pos] MOD 256);
      IF (b = esc) AND (escape = true) THEN
        BEGIN
        oblock[opos] := b;
        opos := opos + 1;
        IF opos > 511 THEN
          BEGIN
          IF out THEN write(bfile,oblock);
          IF download THEN write(btgtfile,oblock);
          opos := 0;
          END;
        END;

      oblock[opos] := b;
      opos := opos + 1;
      IF opos > 511 THEN
        BEGIN
        IF out THEN write(bfile,oblock);
        IF download THEN write(btgtfile,oblock);
        opos := 0;
        END;
      checksum := ord(uxor(uint(b),uint(checksum)));
      END

    ELSE
      FOR i:=1 TO len DO
        BEGIN
        wbyte(mvr(codearr[i+pos]));
        wbyte(codearr[i+pos] MOD 256);
        END;

    endpacket;
    END; { of PROCEDURE startformat }
    {>>>}


  BEGIN { of PROCEDURE output_data }
  c := codelen;
  pos := 0;
  WHILE c > smax DO
    BEGIN
    sformat (pos,smax);
    pos := pos+smax;
    c := c - smax;
    END;

  IF c > 0 THEN sformat(pos,c);
  END; { of PROCEDURE output_data }


  {<<<}
  PROCEDURE procesd;


    PROCEDURE doesd;

    VAR
      sect,ty : byte;
      s : sym_name;
      b : boolean;
      patch, i:integer;
      spt : sym_ptr;
      r : resolve_ptr;

    BEGIN
    ty := gbyte;
    sect := ty MOD 16;
    ty := ty DIV 16;

    CASE ty OF
      0: BEGIN { no idea !! }
         bpos := bpos + 4;

         i := getint;
         esdarr[topesd] := i;
         esdsymarr [topesd] := NIL;

         outaddr[topesd] := esdarr[topesd];
         topesd := topesd + 1;
         END;

      1: BEGIN { common area symbol }
         s := getnam;
         bpos := bpos + 4; {skip int}

         b := find_insert(s,spt,false);
         IF NOT b THEN
           BEGIN
           showmod;
           writeln('internal consistency check failure - lost symbol');
           END;

         esdarr[topesd] := spt^.saddr + baseaddr[spt^.ssect];
         esdsymarr [topesd] := spt;

         outaddr[topesd] := esdarr[topesd];
         topesd := topesd + 1;
         END;

      2,3 : BEGIN { section symbol }
            i := getint;
            esdarr[sect+1] := baseaddr[sect] + sbase[sect];
            esdsymarr [topesd] := NIL;

            outaddr[sect+1] := esdarr[sect+1];
            IF modules THEN
              write(mod_file,' ',sect:2,':',hex(esdarr[sect+1],6,6),'+',
                    hex(i,6,6));
            sbase[sect] := sbase[sect] + i;
            IF odd(sbase[sect]) THEN sbase[sect] := sbase[sect] + 1;
            END;

      4,5 : IF use_history THEN
              BEGIN { symbol defintion, use to make patches on second pass }
              s := getnam;
              b := find_insert(s,spt,FALSE); { find it }

              IF spt^.reslist <> nil THEN
                BEGIN
                r := spt^.reslist;
                REPEAT
                  BEGIN
                  patch := spt^.saddr + baseaddr[spt^.ssect] + r^.res_offset;
                  IF debug THEN writeln ('patching ',hex(r^.res_addr,6,6),
                                         ' with ',
                                         hex(patch-r^.res_offset,6,6), ' + ',
                                         hex(r^.res_offset,6,6));

                  codestart := r^.res_addr;
                  codearr [1] := mvr(mvr(patch));
                  codearr [2] := patch;
                  codelen := 2;
                  output_data;

                  r := r^.next_res;
                  END UNTIL r=nil;
                END;

              bpos := bpos + 4; { skip past offset into module }
              END
            ELSE
              bpos := bpos + 14; { skip past offset into module }

      6,7 : BEGIN { symbol reference }
            s := getnam;
            b := find_insert(s,spt,false);
            IF NOT b THEN
              BEGIN
              showmod;
              writeln('internal check failure - lost symbol');
              END;

            esdarr[topesd] := spt^.saddr + baseaddr[spt^.ssect];
            esdsymarr [topesd] := spt;

            outaddr[topesd] := esdarr[topesd];
            topesd := topesd + 1;
            END;

      8,9 : bpos := bpos + 5;
      10  : bpos := bpos + 15;
      END;
    END; { pof PROCEDURE doesd }

  BEGIN { of PROCEDURE procesd }
  bpos := 2;
  WHILE bpos < o.length DO doesd;
  END; { of PROCEDURE procesd }
  {>>>}
  {<<<}
  PROCEDURE proctxt;

  VAR
    bitmap,curresd:integer;


  PROCEDURE procbyte;

  VAR
    longwd : boolean;
    offset,add,i,numesds,offsize : integer;
    thisesd,w:integer;
    flag : byte;


    PROCEDURE adddata(w:integer);

    BEGIN
    duffer := w = %x'4EBA';
    codelen := codelen + 1;
    codearr[codelen] := w;
    END; { of PROCEDURE adddata }


  BEGIN { of PROCEDURE procbyte }
  IF bitmap >= 0 THEN
    BEGIN
    adddata (mvl (ord (o[bpos])) + ord(o[bpos+1]));
    bpos := bpos+2;
    END

  ELSE
    BEGIN
    IF duffer THEN
      BEGIN
      showmod;
      writeln('Warning - possible assembler foul-up');
      END;

    flag := gbyte;
    numesds := flag DIV 32;
    offsize := flag MOD 8;
    {    writeln('num esds, ',numesds,'  offset size ',offsize);}
    longwd := ((flag DIV 8) MOD 2) = 1;

    add := 0;
    FOR i := 1 TO numesds DO
      BEGIN
      thisesd := gbyte;
      IF thisesd > topesd THEN
        BEGIN
        showmod;
        writeln(' assembler foul-up.. trying to use an undefined ESD : '
                ,thisesd);
        END;

      IF odd(i) THEN
        add := add + esdarr[thisesd]
      ELSE
        add := add - esdarr[thisesd];
      END;

    offset := 0;
    FOR i := 1 TO offsize DO offset := mvl(offset) + gbyte;
    CASE offsize OF
      0,4:;
      1: IF offset > 127   THEN offset := int (uor (uint (offset),%X'FFFFFF00'));
      2: IF offset > 32767 THEN offset := int (uor (uint (offset),%X'FFFF0000'));
      END;
    {    writeln('OFFSET ',hex(add,6,6),'+',hex(offset,6,6),'=',hex(add+offset,6,6));
    }
    add := add + offset;
    IF numesds = 0 THEN
      BEGIN
      IF odd(offset) THEN
        BEGIN
        showmod;
        writeln ('odd fix-up offset - assembler error .',offset,curresd);
        writeln ('>>',hex(codestart,6,6));
        offset := offset + 1;
        END;

      IF codelen > 0 THEN output_data;
      outaddr[curresd] := outaddr[curresd] + codelen*2 + offset;
      codelen := 0;
      codestart := outaddr[curresd];
      END

    ELSE  { numesd <> 0 }
      BEGIN
      IF NOT longwd THEN
        BEGIN
        IF (add > 32767) OR (add < -32768) THEN
          BEGIN
          showmod;
          writeln('Long address generated into word location :',hex(add,8,8));
          END;
        END;

      IF esdsymarr [thisesd] <> NIL THEN { only need named symbols }
        IF mod_id <> esdsymarr [thisesd]^.mname THEN { outside module }
          BEGIN
          IF history THEN {--- address to be resolved LONGWORD only at present}
            add_res(esdsymarr [thisesd], codestart + codelen*2, offset);
          IF debug THEN Writeln ('sym ',longwd,' ',thisesd:2,' ',
                   esdsymarr [thisesd]^.sname,' ',
                   hex(add,8,8), ' = ', hex(esdarr[thisesd]), ' + ',
                   hex(offset,4,4), ';', hex(offsize,1,1),
                   ' at ', hex (codestart + codelen*2,8,8));
          END;

      {--- generate resolved address }
      IF longwd THEN adddata (mvr (mvr (add)));
      adddata(add);
      END;
    END;

  bitmap := bitmap*2;
  END; { of PROCDURE procbyte }


  BEGIN { of PROCEDURE proctxt }
  bpos := 2;
  bitmap := getint;

  codelen := 0;
  curresd := gbyte;
  codestart := outaddr[curresd];

  WHILE bpos < o.length DO procbyte;

  output_data;
  {--- dont forget convert to bytes}
  outaddr[curresd] := outaddr[curresd]+(codelen*2);
  END; { of PROCEDURE proctxt }
  {>>>}
  {<<<}
  PROCEDURE proceom;

  BEGIN
  IF modules THEN writeln(mod_file);
  END; { of PROCEDURE proceom }
  {>>>}

BEGIN { of PROCEDURE procrec2 }
CASE o[1] OF
  '1': procid;
  '2': procesd;
  '3': proctxt;
  '4': proceom;
  END;
END; { of PROCEDURE procrec2 }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
PROCEDURE openoutput;

BEGIN
opos := 0;
inpacket := false;

IF bin THEN
  BEGIN
  IF download THEN
    BEGIN
    open (btgtfile,'sys$target',history:=new);
    writeln( 'Downloading binary file ', get_nam( pas$fab( btgtfile )));
    rewrite (btgtfile);
    END;

  IF out THEN
    BEGIN
    open (bfile,cmdroot+'.BIN',history:=new);
    IF chat OR debug OR (NOT quiet) THEN
      writeln ('Making binary file ', get_nam( pas$fab( bfile )));
    IF log THEN
      writeln (log_file, 'Making binary file ', get_nam( pas$fab( bfile )));
    rewrite (bfile);
    END;
  END

ELSE
  BEGIN
  IF download THEN
    BEGIN
    open (tgtfile,'sys$target',history:=new);
    writeln( 'Downloading SR file ', get_nam( pas$fab( tgtfile )));
    rewrite (tgtfile);
    END;

  IF out THEN
    BEGIN
    open (sfile,cmdroot+'.SR',history:=new);
    writeln( 'Making SR file ', get_nam( pas$fab( sfile )));
    rewrite (sfile);
    END;

  END;

END; { of PROCEDURE openoutput }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE sendstop;

VAR
  endstr : string;
  I : integer;

BEGIN
IF bin THEN
  BEGIN
  checksum := 0;
  binbyte(esc);
  binbyte(0);
  wbyte(2);
  wbyte(0);
  wbyte(4);
  FOR i := 1 TO 4 DO wbyte(0);
  endpacket;
  FOR i := 0 TO 511 DO binbyte(0);
  END

ELSE
  BEGIN
  endstr := 'S9030000FC';
  sendsform (endstr);
  sendsfnewline;
  END;

END; { of PROCEDURE sendstop }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE closeoutput;

BEGIN
IF inpacket THEN endpacket;
sendstop;

IF bin THEN
  BEGIN
  IF download THEN close (btgtfile);
  IF out THEN close (bfile);
  END
ELSE
  BEGIN
  IF download THEN close (tgtfile);
  IF out THEN close (sfile);
  END;

END;
{>>>}

{<<<}
{-------------------------------------------------------------------------}
PROCEDURE do_switch (start,sw_len: integer);
{--- processes a switch setting directive}

VAR
  noflag : boolean;
  sw : PACKED ARRAY [1..3] OF char;
  endpos, sw_end, sect, i, swpos : integer;
  c : char;


  PROCEDURE setsw(VAR b:boolean);

  BEGIN
  b := NOT noflag;
  END; { of PROCEDURE setsw }

  FUNCTION gnext:char;

  BEGIN
  gnext := line_buff [swpos];
  IF swpos < sw_len THEN swpos:=swpos+1;
  END; { of PROCEDURE gnext }


BEGIN
{--- Convert to lower case only}
FOR i:=1 TO sw_len DO
  IF (line_buff[i]>='A') AND (line_buff[i]<='Z') THEN
    line_buff[i]:=chr(ord(line_buff[i])+ord('a')-ord('A'));

swpos := start;
REPEAT
  noflag := false;
  sw[1] := gnext;
  sw[2] := gnext;
  sw[3] := '.';
  IF sw = 'no.' THEN
    BEGIN
    noflag := true;
    sw[1] := gnext;
    sw[2] := gnext;
    END;

  sw[3] := gnext;
  sw_end := swpos;

  REPEAT
    c := gnext;
    UNTIL (c='/') OR (null(c)) OR (swpos>=sw_len); {skip to next switch}

  IF (sw[1]='o') AND (sw[2]>='0') AND (sw[2]<='9') THEN
    BEGIN
    {we're processing a section start address record}
    IF sw[3]=':' THEN sect:=ord(sw[2])-ord('0') ELSE
      BEGIN
      sect:=10*(ord(sw[2])-ord('0'))+(ord(sw[3])-ord('0'));
      sw_end:=sw_end+1;
      END;

    IF swpos <> sw_len THEN endpos := swpos - 2 ELSE endpos:=swpos;

    IF (sect >= 0) AND (sect <= 15) THEN
      BEGIN
      userbase[sect] := 0;
      FOR i := sw_end TO endpos DO
        userbase[sect] := 16 * userbase[sect] + hexchr(line_buff[i]);
      IF debug THEN writeln(hex(userbase[sect],6,6),' ',sect);
      END
    ELSE writeln(' Illegal section number in switch ''',sw,'''');

    END

  ELSE
    IF (sw='xre') OR (sw='xrf') THEN setsw(xref)        {generate xref file}
    ELSE IF sw='map' THEN setsw(map)                    {generate map file}
    ELSE IF sw='sym' THEN setsw(symout)                 {generate symbol file}
    ELSE IF sw='bin' THEN setsw(bin)                    {binary output}
    ELSE IF sw='mod' THEN setsw(modules)                {module list}
    ELSE IF sw='deb' THEN setsw(debug)                  {debug mode}
    ELSE IF (sw='dld') OR (sw='dow') THEN setsw(download){download to target}
    ELSE IF sw='out' THEN setsw(out)                    {generate any output at all!}
    ELSE IF sw='cha' THEN setsw(chat)                   {generate loads of output}
    ELSE IF sw='qui' THEN setsw(quiet)                  {generate minimum output}
    ELSE IF sw='eng' THEN setsw(english)                {say understandable things}
    ELSE IF sw='log' THEN                               {put sizes etc in .log file}
      BEGIN
      setsw(log);
      open_log_file;
      END
    ELSE IF sw='fil' THEN setsw(files)                  {generate put filenames in mod file}
    ELSE IF sw='his' THEN setsw(history)                {generate history file}
    ELSE IF sw='bel' THEN
      BEGIN
      setsw(bell);                              {generate bells at end of link}
      IF noflag THEN writeln('What do you want, a prize or something??');
      END
    ELSE IF sw='che' THEN setsw(check)                  {check all possible grubbies}
    ELSE IF sw='esc' THEN setsw(escape)                 {replace all 1B's in code with 1B1B}
    ELSE writeln('Unknown switch :''',sw,'''');
    UNTIL (swpos>=sw_len) OR (null(c));

END; { of PROCEDURE do_switch }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE switchprocess(VAR s: string);

VAR
  i,j,l:integer;
  slashfound:boolean;

BEGIN
l:=length(s);
i:=1;
slashfound:=false;

WHILE (i<l) AND NOT slashfound DO
  IF s[i]='/' THEN slashfound:=true ELSE i:=i+1;

IF slashfound THEN
  BEGIN
  FOR j:=i+1 TO l DO
    line_buff[j-i]:=s[j]; {pick up everything after the slash }

  do_switch(1,l-i);
  IF i=1 THEN s:='' ELSE s:=substr(s,1,i-1);
  END;

END; { of PROCEDURE switchprocess }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
FUNCTION getfile (VAR terminator : char) : filename;

VAR
  f : filename;
  c : char;
  s : string;
  tempfile: text_ptr_t;

BEGIN
f := '';
REPEAT
  { --- Get the next char that is part of a filename. }
  REPEAT
    { --- Read the next char. }
    read(cmdfile^.f,c);

    { --- If it's at the start of a line, handle switches and comments. }
    IF newline THEN
      BEGIN
      newline := false;

      { --- Read sequence of comment/control lines. }
      WHILE ((c='!') OR (c='/') OR (c = '@')) AND NOT eof( cmdfile^.f )
             DO {its a comment/control line}
        BEGIN
        readln(cmdfile^.f,s);

        IF c = '@' THEN
          BEGIN
          {writeln( 'Line read is ', s );}

          WHILE NOT (s[ s.length ] IN [ 'A' .. 'Z', 'a' .. 'z',
                        '0' .. '9', '-', '_' ]) DO
            s.length := s.length - 1;

          writeln( 'File read is ', s );

          NEW( tempfile );
          tempfile^.next_text := cmdfile;
          cmdfile := tempfile;
          open(cmdfile^.f,s,history:=readonly,sharing:=readonly);

          writeln( 'File opened is ', get_nam( pas$fab( cmdfile^.f )));

          reset(cmdfile^.f);

          END
        ELSE IF (c='/') AND (pass=1) THEN
          BEGIN
          s := '/'+s;
          switchprocess(s);
          END;

        IF NOT eof( cmdfile^.f ) THEN read(cmdfile^.f,c);
        END{WHILE};

      IF eof( cmdfile^.f ) THEN
        IF cmdfile^.next_text <> NIL THEN
          BEGIN
          close( cmdfile^.f );
          cmdfile := cmdfile^.next_text;
          END;

      END{IF newline};

    { --- If we're at the end of the line, absorb empty lines. }
    IF eoln(cmdfile^.f) THEN
      BEGIN
      readln(cmdfile^.f);
      WHILE eoln(cmdfile^.f) AND NOT eof(cmdfile^.f) DO readln(cmdfile^.f);
      newline := true;
      END;
  UNTIL (NOT null(c)) OR eof(cmdfile^.f);

  IF eof( cmdfile^.f ) THEN
    IF cmdfile^.next_text <> NIL THEN
      BEGIN
      close( cmdfile^.f );
      cmdfile := cmdfile^.next_text;
      END;

  IF (c <> '=') AND (c <> ',') AND (c <> '!') AND NOT null(c) THEN
    BEGIN
    f.length := f.length+1;
    f[f.length] := c;
    END;
UNTIL (c = ',') OR (c = '=') OR eof(cmdfile^.f);

IF eof( cmdfile^.f ) THEN
  IF cmdfile^.next_text <> NIL THEN
    BEGIN
    close( cmdfile^.f );
    cmdfile := cmdfile^.next_text;
    END;

getfile := f;
terminator := c;
{writeln( 'getfile returning <', f, '>, terminator <', c, '>' );}
END; { of PROCEDURE getfile }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
FUNCTION getext (VAR f: filename; dext: filename) : filename;

VAR
  i:integer;

BEGIN
i := f.length;
{search backwards, to see if first non-alpha char is a '.' (i.e. extension)}

WHILE (i > 1) AND alphanum(f[i]) DO i := i-1;

IF f[i]='.' THEN
  getext := substr(f,i,f.length+1-i)
ELSE
  BEGIN
  getext := dext;
  f := f + dext;
  END;

END; { of PROCEDURE getext }
{>>>}

{<<<}
{-------------------------------------------------------------------------}
FUNCTION current_time : milestone;

VAR
  ms : milestone;
  temp :integer;

  FUNCTION get_num( startch: integer ): integer;
  VAR
  temp1, temp2: integer;
  BEGIN
  temp1 := ORD( ms.time_of_day[ startch ] ) - ORD( '0' );
  temp2 := ORD( ms.time_of_day[ startch + 1 ] ) - ORD( '0' );
  get_num := (temp1 * 10) + temp2;
  END;

BEGIN
ms.mill_time := clock;
time(ms.time_of_day);

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

current_time := ms;
END; { of FUNCTION current_time }
{>>>}
{<<<}
{-------------------------------------------------------------------------}
PROCEDURE show_milestone (s: string; ms1,ms2: milestone);
VAR
temp, cc, ss, mm, hh: integer;
time_str: string;
BEGIN
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
writev ( time_str, hh :2, ':', mm :2, ':', ss :2, '.', cc :2 );
FOR temp := 1 TO length( time_str ) DO
  IF time_str[ temp ] = ' ' THEN time_str[ temp ] := '0';
write( ' ', time_str );
writeln ('  ',((ms1.mill_time-ms2.mill_time)*100)/
         (end_link.mill_time-start_link.mill_time):7:2,'%');
END;
{>>>}
{<<<}
PROCEDURE hash_usage;
VAR
depth, i, hash_used: integer;
depth_used: ARRAY[ 0 .. 10 ] OF integer;
s_ptr: sym_ptr;
BEGIN
hash_used := 0;
FOR depth := 0 TO 10 DO depth_used[ depth ] := 0;
FOR i := 0 TO max_hash DO IF hash_table[i] <> nil THEN
  BEGIN
  hash_used := hash_used + 1;
  depth := 0;
  s_ptr:=hash_table[i];
  REPEAT
    depth := depth + 1;
    s_ptr:=s_ptr^.next_sym;
  UNTIL s_ptr=nil;
  IF depth > 10 THEN
    BEGIN
    writeln( 'Depth ', depth :0, ' too deep, rounding to 10.' );
    depth := 10;
    END;
  depth_used[ depth ] := depth_used[ depth ] + 1;
  END;
IF chat OR debug THEN
  BEGIN
  writeln(hash_used :0, ' out of ', max_hash :0, ' hash table entries used.' );
  FOR depth := 0 TO 10 DO
    IF depth_used[ depth ] <> 0 THEN
      write( depth_used[ depth ] :0, ' @ ', depth :0, ',' );
  writeln;
  END; {chatting}
END;
{>>>}

{<<<}
BEGIN
{--- set up default switch settings}
xref := false;
symout := false;
bin := false;
check := false;
bell := false;
out := true;
map := true;

history := FALSE;
chat := FALSE;

use_history := FALSE;

modules := false;
files := false;
file_id := 'no file open.' ;

quiet := false;
english := false;
log := false;
newline := true;

escape := true;

{--- set up pointers for common area list}
com_head := nil;
prev_com := nil;

defext := '.ro';

total := 0;
undefsym := 0;
init_hash;

FOR i:=-1 TO 15 DO
  BEGIN
  sectbase[i] := 0;
  userbase[i] := -1;  {set up user bases as not needed}
  END;

prompt := 'quicklink '+vermsg;
IF chat OR debug THEN
  writeln( prompt );

fil_len := -1;
P_getcmdline (fil, fil_len);
switchprocess (fil);

cmdname := fil;
ext := getext (cmdname,'.cmd');
cmdroot := cmdname;
cmdroot.length := cmdname.length - ext.length;

colonpos := index(cmdroot,':');
dirend := index(cmdroot,']');
IF colonpos > dirend THEN
  dirend := colonpos;
IF dirend <> 0 THEN
  cmdroot := substr (cmdroot,dirend+1,cmdroot.length-dirend);

start_link := current_time;

start_read_his := current_time;
end_read_his := start_read_his;

NEW( cmdfile );
cmdfile^.next_text := NIL;

IF chat OR debug THEN
  writeln( 'File given is ', cmdname );

open (cmdfile^.f,cmdname,history:=readonly,sharing:=readonly);
reset (cmdfile^.f);

full_filename := get_nam (pas$fab (cmdfile^.f));
writeln ( 'Linking from ', full_filename);

pass := 1;
firstfile := true;
basepos := startbase;

REPEAT
  f := getfile (termchar);
  IF termchar = '=' THEN
    BEGIN
    IF firstfile THEN
      cmdroot := f
    ELSE
      writeln('You can''t put an ''='' THERE!');
    END

  ELSE {process the file}
    BEGIN
    ext := getext (f, defext);

    IF ext = '.his' THEN { history file of previous link }
      BEGIN
      IF use_history THEN
         Writeln ('Can only use one history file, subsequent ones ignored')
      ELSE
        BEGIN
        start_read_his := current_time;
        read_history (f);
        end_read_his := current_time;
        END;
      use_history := TRUE;
      END

    ELSE IF ext = '.rx' THEN { text format .rx file }
      BEGIN
      opentextin (f);
      REPEAT
        gettextrec (o);
        IF o.length > 0 THEN
          processrec;
      UNTIL eof (tobjfile) ;
      close (tobjfile);
      END

    ELSE { normal .ro file }
      BEGIN
      openin (f);
      REPEAT
        getrec (o);
        IF o.length > 0 THEN processrec;
      UNTIL empty;
      close (objfile);
      END;
    END;

  firstfile := false;
  UNTIL eof(cmdfile^.f);

end_pass_1 := current_time;
alloc_com;

hash_usage;
IF chat OR debug OR (NOT quiet) THEN
  BEGIN
  writeln (numsymbols:5,' in symbol table');
  IF log THEN
    writeln (log_file, numsymbols:5,' in symbol table');
  END;

baseaddr[-1] := 0;      {set up base of absolute section}

FOR i := 0 TO 15 DO
  BEGIN
  IF userbase[i] <> -1 THEN {--- put this section somewhere special}
    basepos := userbase[i];

  IF sectbase[i] <> 0 THEN
    BEGIN
    IF NOT english THEN
      write ('Section ',i:2,' Start ',hex(basepos,6,6),' Length ', hex(sectbase[i],6,6));
    baseaddr[i] := basepos;
    basepos := basepos+sectbase[i];
    IF NOT english THEN
      writeln (' Finish  ',hex(basepos,6,6));
    END;
  END;

IF english THEN
  BEGIN
  writeln;
  IF sectbase[8] <> 0 THEN
    BEGIN
    writeln ('Size of P                 (8)  = ', sectbase[8]:8, ' bytes'); total := total + sectbase[8];
    END;
  IF sectbase[9] <> 0 THEN
    BEGIN
    writeln ('Size of HELP              (9)  = ', sectbase[9]:8, ' bytes');
    total := total + sectbase[9];
    END;
  IF sectbase[12] <> 0 THEN
    BEGIN
    writeln ('Size of error messages   (12)  = ', sectbase[12]:8, ' bytes');
    total := total + sectbase[12];
    END;
  IF sectbase[13] <> 0 THEN
    BEGIN
    writeln ('Size of code & constants (13)  = ', sectbase[13]:8, ' bytes');
    total := total + sectbase[13];
    END;
  IF sectbase[14] <> 0 THEN
    BEGIN
    writeln ('Size of diagnostic block (14)  = ', sectbase[14]:8, ' bytes');
    total := total + sectbase[14];
    END;
  IF sectbase[15] <> 0 THEN
    BEGIN
    writeln ('Size of global variables (15)  = ', sectbase[15]:8, ' bytes');
    total := total + sectbase[15];
    END;
  writeln ('Total size                     = ', total:8, ' bytes');
  END;

overlap_check;
end_space_alloc:=current_time;

IF undefsym<>0 THEN
  BEGIN
  writeln('Number of undefined symbols:- ',undefsym);
  IF log THEN
    writeln (log_file, 'Number of undefined symbols:- ',undefsym);
  check_undef;
  END;

IF bin THEN
  smax := 512 {randomly large number!}
ELSE
  smax := 16; {s-format max line size}

IF modules OR out OR download THEN
  BEGIN
  pass := 2;
  openoutput;
  reset (cmdfile^.f);
  newline := true;

  IF modules THEN
    BEGIN
    open (mod_file, cmdroot+'.MOD', history := new);
    rewrite(mod_file);
    END;

  FOR i:=0 TO 15 DO sbase[i]:=0;
    REPEAT
      f := getfile(termchar);

      IF termchar<>'=' THEN
        BEGIN
        ext := getext (f,defext);
        IF ext = '.his' THEN
          BEGIN
          { do not reread history file }
          END

        ELSE IF ext = '.rx' THEN { text format .rx file }
          BEGIN
          opentextin (f);
          REPEAT
            gettextrec (o);
            IF o.length > 0 THEN procrec2;
          UNTIL eof (tobjfile) ;
          close (tobjfile);
          END

        ELSE {normal .ro file}
          BEGIN
          openin(f);
          REPEAT
            getrec(o);
            IF o.length > 0 THEN procrec2;
          UNTIL empty;
          close(objfile);
          END;

        END;
      UNTIL eof(cmdfile^.f);

  IF modules THEN
    close(mod_file);
  closeoutput;
  END;

end_pass_2 := current_time;

IF history THEN
  disp_history;
end_his_gen := current_time;

IF symout THEN
  gen_sym_file;
end_sym_gen := current_time;

IF map THEN
  disp_hash;
end_map_gen := current_time;

IF xref THEN
  disp_xref;
end_xref_gen := current_time;

IF bell THEN FOR i := 1 TO 10 DO
  write (chr(7));
writeln;
end_link := current_time;

IF chat OR debug OR (NOT quiet) THEN
  BEGIN
  writeln ('Link started           ',start_link.time_of_day);
  show_milestone ('Pass 1                 ',end_pass_1,start_link);
  IF start_read_his.mill_time <> end_read_his.mill_time THEN
    show_milestone ('Reading history file   ', end_read_his, start_read_his );
  show_milestone ('Space allocation       ',end_space_alloc,end_pass_1);
  show_milestone ('Pass 2                 ',end_pass_2,end_space_alloc);
  IF history THEN
    show_milestone ('.HIS generation        ',end_his_gen,end_pass_2);
  IF symout THEN
    show_milestone ('.SYM generation        ',end_sym_gen,end_his_gen);
  IF map THEN
    show_milestone ('.MAP generation        ',end_map_gen,end_sym_gen);
  IF xref THEN
    show_milestone ('.XRF generation        ',end_xref_gen,end_map_gen);
  show_milestone ( 'Link ended             ',end_link,start_link);
  writeln;
  writeln ('total CPU time:- ', (end_link.mill_time-start_link.mill_time)/1000:7:2);
  END;

IF english THEN
  BEGIN
  date (datestring);
  writeln;
  writeln ('Link started ', start_link.time_of_day, ' ', datestring);
  writeln ('Link ended   ', end_link.time_of_day, ' ', datestring);
  END;

IF log THEN
  close_log_file;
END.
{>>>}
