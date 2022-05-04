{ dumpToken.pas - dump tokens from token.tmp file }
%include 'common.def';
%include 'token.def';

type
  tokenlengthtable = array [tokentype] of 0..10; {defines token lengths}

var
  tokenSharedPtr: tokenSharedPtrType;
  tokencount: integer;
  tokenbufindex: 0..diskbufsize;
  listingname: packed array [1..255] of char;
  toklengths: tokenlengthtable;

  {<<<}
  procedure getnexttoken;

  var
    t: tokentype;

    {<<<}
    procedure gettempfile;

    begin
      if tokenbufindex = diskbufsize then
        begin
        tokenbufindex := 0;
        get (tokenSharedPtr^.tokenFile);
        end
      else
        tokenbufindex := tokenbufindex + 1;
    end;
    {>>>}
    {<<<}
    function getint: integer;
    { Get an integer value from the file }

    var
      j: 1..32;

      { This fudges an integer into bytes.  The constant "32" is large enough number to include all probable systems. }
      fudge:
        record
          case boolean of
            true : (int: integer);
            false: (byte: packed array [1..32] of hostfilebyte);
        end;

    begin
      fudge.int := tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte;
      gettempfile;
      if fudge.int = hostfilelim then
        for j := 1 to hostintsize * hostfileunits do
          begin
          fudge.byte[j] := tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte;
          gettempfile;
          end;
      getint := fudge.int;
    end;
    {>>>}
    {<<<}
    procedure getreal (var r: realarray);

    var
      j: 1..32;

      { this fudges a real into bytes.  The constant "32" is large enough number to include all probable systems. }
      fudge:
        record
          case boolean of
            true : (rl: realarray);
            false: (byte: packed array [1..32] of hostfilebyte);
        end;


    begin
      for j := 1 to size(realarray) do
        begin
        fudge.byte[j] := tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte;
        gettempfile;
        end;

      r := fudge.rl;
    end;
    {>>>}

  begin
    with tokenSharedPtr^.nexttoken do
      begin
      repeat
        t := tokenSharedPtr^.tokenFile^.block[tokenbufindex].toke;
        gettempfile;
        if t = newfile then
          begin
          baseline := getint;
          fileIndex := getint; { stringtable index of filename }
          end
        else if t = lineinc then
          line := line + 1
        else if t = lineadd then
          begin
          line := line + tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte;
          gettempfile;
          end
        until not (t in [lineinc, lineadd, newfile]);

      token := t;

      left := tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte;
      gettempfile;

      if t in [ident, intconst, charconst, realconst, dblrealconst, stringconst] then
        begin
        right := tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte;
        gettempfile;
        case t of
          {<<<}
          ident:
            begin
            key := getint;
            keyPos := getint;
            end;
          {>>>}
          {<<<}
          intconst:
            intvalue := getint;
          {>>>}
          {<<<}
          charconst:
            begin
            tokenSharedPtr^.tokenFile^.block[tokenbufindex].byte;
            gettempfile;
            end;
          {>>>}
          realconst,
          {<<<}
          dblrealconst:
            getreal(realvalue);
          {>>>}
          {<<<}
          stringconst:
            begin
            pos := getint;
            len := getint;
            end;
          {>>>}
          end
        end
      else
        right := left + toklengths[t];
      end
  end;
  {>>>}
  {<<<}
  procedure printreal (r: realarray);

  type
    hexdigit = 0..15;

  var
    j: 1..32;

    { this fudges a real into bytes.  The constant "32" is simply a large enough number to include all probable systems. }
    fudge:
      record
        case integer of
          1: (rl: realarray);
          2: (rr: real);
          3: (byte: packed array [1..32] of hostfilebyte);
      end;

    {<<<}
    procedure writehex (i: hexdigit);

    begin
      if i < 10 then
        write (chr (i + ord ('0')))
      else
        write (chr (i - 10 + ord ('A')));
    end;
    {>>>}

  begin
    fudge.rl := r;
    write (fudge.rr: 21, ' (');
    for j := 1 to size (realarray) do
      begin
      writehex (fudge.byte[j] div 16);
      writehex (fudge.byte[j] mod 16);
      end;
    write(')');
  end;
  {>>>}

begin
  new (tokenSharedPtr);

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
  toklengths[beginsym] := 4;
  toklengths[ifsym] := 1;
  toklengths[casesym] := 3;
  toklengths[whilesym] := 4;
  toklengths[repeatsym] := 5;
  toklengths[forsym] := 2;
  toklengths[withsym] := 3;
  toklengths[gotosym] := 3;
  toklengths[usesym] := 2;
  toklengths[definesym] := 5;
  toklengths[sharedsym] := 5;

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
  tokencount := 0;
  tokenbufindex := 0;
  tokenSharedPtr^.nexttoken.line := 1;

  reset (tokenSharedPtr^.tokenFile, 'token.tmp');
  getnexttoken;

  writeln (' Base  Line Left Right   pos index Id Token');
  repeat
    tokencount := tokencount + 1;
    with tokenSharedPtr^.nexttoken do
      begin
      write (baseline:5, ' ', line:5, ' ', left:4, ' ', right:5, ' ',filePos:5, ' ', fileIndex:5, ' ', ord(token):2, ' ');
      {<<<  write token names}
      case token of
        programsym:   write ('PROGRAM');
        labelsym:     write ('LABEL');
        constsym:     write ('CONST');
        typesym:      write ('TYPE');
        varsym:       write ('VAR');
        proceduresym: write ('PROCEDURE');
        functionsym:  write ('FUNCTION');
        otherwisesym: write ('OTHERWISE');
        uparrow:      write ('^');
        arraysym:     write ('ARRAY');
        filesym:      write ('FILE');
        setsym:       write ('SET');
        packedsym:    write ('PACKED');
        recordsym:    write ('RECORD');
        stringsym:    write ('STRING');
        univsym:      write ('UNIV');
        beginsym:     write ('BEGIN');
        ifsym:        write ('IF');
        casesym:      write ('CASE');
        whilesym:     write ('WHILE');
        repeatsym:    write ('REPEAT');
        forsym:       write ('FOR');
        withsym:      write ('WITH');
        gotosym:      write ('GOTO');
        usesym:       write ('USE');
        definesym:    write ('DEFINE');
        sharedsym:    write ('SHARED');
        eql:          write ('=');
        lss:          write ('<');
        gtr:          write ('>');
        neq:          write ('<>');
        leq:          write ('<=');
        geq:          write ('>=');
        insym:        write ('IN');
        plus:         write ('+');
        minus:        write ('-');
        orsym:        write ('OR');
        star:         write ('*');
        slash:        write ('/');
        divsym:       write ('DIV');
        modsym:       write ('MOD');
        andsym:       write ('AND');
        ofsym:        write ('OF');
        endsym:       write ('END');
        elsesym:      write ('ELSE');
        thensym:      write ('THEN');
        dosym:        write ('DO');
        untilsym:     write ('UNTIL');
        tosym:        write ('TO');
        downtosym:    write ('DOWNTO');
        notsym:       write ('NOT');
        nilsym:       write ('NIL');
        colon:        write (':');
        dot:          write ('.');
        dotdot:       write ('..');
        comma:        write (',');
        semicolon:    write (';');
        becomes:      write (':=');
        lpar:         write ('(');
        rpar:         write (')');
        lbrack:       write ('[');
        rbrack:       write (']');
        intconst:     write ('INTCONST value:', intvalue:1);
        realconst:    begin
                      write ('REALCONST value: ');
                      printreal (realvalue);
                      end;
        dblrealconst: begin
                      write ('DBLREALCONST value: ');
                      printreal (realvalue);
                      end;
        charconst:    write ('CHARCONST value:', intvalue:1);
        stringconst:  write ('STRINGCONST pos:', pos:1, ' len:',len:1);
        ident:        write ('IDENT key:', key:1);
        otherwise     write ('unknownToken - ', ord(token):1);
        end;
      {>>>}
      writeln;
      end;

    getnexttoken;
  until (tokenSharedPtr^.nexttoken.token = eofsym) or eof (tokenSharedPtr^.tokenFile);

  with tokenSharedPtr^.nexttoken do
    writeln (baseline:5, line:5, ' ', left:4, ' ', right:4, filePos:5, fileIndex:5, ord(token):3, ' EOF');
  close (tokenSharedPtr^.tokenFile);

  writeln (tokencount: 1, ' tokens');
  close (output);
end.
