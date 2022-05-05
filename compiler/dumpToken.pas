{ dumpToken.pas - dump tokens output.token file }
program dumpToken (output);
%include 'common.def';
%include 'token.def';

type
  tokenLengthTable = array [tokentype] of 0..10;

var
  tokenSharedPtr: tokenSharedPtrType;
  tokenLengths: tokenLengthTable;
  tokenCount: integer;

  {<<<}
  procedure getNextToken;

  var
    t: tokentype;

    {<<<}
    function getInt: integer;
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
      fudge.int := tokenSharedPtr^.tokenFile^.byte;
      get (tokenSharedPtr^.tokenFile);
      if fudge.int = hostfilelim then
        for j := 1 to hostintsize * hostfileunits do
          begin
          fudge.byte[j] := tokenSharedPtr^.tokenFile^.byte;
          get (tokenSharedPtr^.tokenFile);
          end;
      getInt := fudge.int;
    end;
    {>>>}
    {<<<}
    procedure getReal (var r: realarray);

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
        fudge.byte[j] := tokenSharedPtr^.tokenFile^.byte;
        get (tokenSharedPtr^.tokenFile);
        end;

      r := fudge.rl;
    end;
    {>>>}

  begin
    with tokenSharedPtr^.nexttoken do
      begin
      repeat
        t := tokenSharedPtr^.tokenFile^.token;
        get (tokenSharedPtr^.tokenFile);
        if t = newfile then
          begin
          baseline := getInt;
          fileIndex := getInt; { stringtable index of filename }
          end
        else if t = lineinc then
          line := line + 1
        else if t = lineadd then
          begin
          line := line + tokenSharedPtr^.tokenFile^.byte;
          get (tokenSharedPtr^.tokenFile);
          end
        until not (t in [lineinc, lineadd, newfile]);

      token := t;

      left := tokenSharedPtr^.tokenFile^.byte;
      get (tokenSharedPtr^.tokenFile);

      if t in [ident, intconst, charconst, realconst, dblrealconst, stringconst] then
        begin
        right := tokenSharedPtr^.tokenFile^.byte;
        get (tokenSharedPtr^.tokenFile);
        case t of
          {<<<}
          ident:
            begin
            key := getInt;
            keyPos := getInt;
            end;
          {>>>}
          {<<<}
          intconst:
            intvalue := getInt;
          {>>>}
          {<<<}
          charconst:
            begin
            tokenSharedPtr^.tokenFile^.byte;
            get (tokenSharedPtr^.tokenFile);
            end;
          {>>>}
          realconst,
          {<<<}
          dblrealconst:
            getReal (realvalue);
          {>>>}
          {<<<}
          stringconst:
            begin
            pos := getInt;
            len := getInt;
            end;
          {>>>}
          end
        end
      else
        right := left + tokenLengths[t];
      end
  end;
  {>>>}
  {<<<}
  procedure printReal (r: realarray);

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
    procedure writeHex (i: hexdigit);

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
      writeHex (fudge.byte[j] div 16);
      writeHex (fudge.byte[j] mod 16);
      end;
    write(')');
  end;
  {>>>}

begin
  new (tokenSharedPtr);

  {<<<  init token lengths}
  tokenLengths[programsym] := 6;
  tokenLengths[labelsym] := 4;
  tokenLengths[constsym] := 4;
  tokenLengths[typesym] := 3;
  tokenLengths[varsym] := 2;
  tokenLengths[proceduresym] := 8;
  tokenLengths[functionsym] := 7;
  tokenLengths[uparrow] := 0;
  tokenLengths[arraysym] := 4;
  tokenLengths[filesym] := 3;
  tokenLengths[setsym] := 2;
  tokenLengths[recordsym] := 5;
  tokenLengths[stringsym] := 5;
  tokenLengths[univsym] := 3;
  tokenLengths[packedsym] := 5;
  tokenLengths[originsym] := 5;
  tokenLengths[beginsym] := 4;
  tokenLengths[ifsym] := 1;
  tokenLengths[casesym] := 3;
  tokenLengths[whilesym] := 4;
  tokenLengths[repeatsym] := 5;
  tokenLengths[forsym] := 2;
  tokenLengths[withsym] := 3;
  tokenLengths[gotosym] := 3;
  tokenLengths[usesym] := 2;
  tokenLengths[definesym] := 5;
  tokenLengths[sharedsym] := 5;

  tokenLengths[eql] := 0;
  tokenLengths[lss] := 0;
  tokenLengths[gtr] := 0;
  tokenLengths[neq] := 1;
  tokenLengths[leq] := 1;
  tokenLengths[geq] := 1;
  tokenLengths[insym] := 1;
  tokenLengths[plus] := 0;
  tokenLengths[minus] := 0;
  tokenLengths[orsym] := 1;
  tokenLengths[star] := 0;
  tokenLengths[slash] := 0;
  tokenLengths[divsym] := 2;
  tokenLengths[modsym] := 2;
  tokenLengths[andsym] := 2;
  tokenLengths[ofsym] := 1;
  tokenLengths[endsym] := 2;
  tokenLengths[elsesym] := 3;
  tokenLengths[thensym] := 3;
  tokenLengths[otherwisesym] := 8;
  tokenLengths[dosym] := 1;
  tokenLengths[untilsym] := 4;
  tokenLengths[tosym] := 1;
  tokenLengths[downtosym] := 5;
  tokenLengths[notsym] := 2;
  tokenLengths[at] := 0;
  tokenLengths[nilsym] := 2;
  tokenLengths[colon] := 0;
  tokenLengths[dot] := 0;
  tokenLengths[dotdot] := 1;
  tokenLengths[comma] := 0;
  tokenLengths[semicolon] := 0;

  tokenLengths[becomes] := 1;

  tokenLengths[lpar] := 0;
  tokenLengths[rpar] := 0;

  tokenLengths[lbrack] := 0;
  tokenLengths[rbrack] := 0;

  tokenLengths[intconst] := 0;
  tokenLengths[realconst] := 0;
  tokenLengths[dblrealconst] := 0;
  tokenLengths[charconst] := 0;
  tokenLengths[stringconst] := 0;

  tokenLengths[ident] := 0;
  tokenLengths[eofsym] := 0;

  tokenLengths[lineinc] := 0;
  tokenLengths[lineadd] := 0;
  tokenLengths[newfile] := 0;
  {>>>}
  tokenCount := 0;
  tokenSharedPtr^.nexttoken.line := 1;

  writeln ('------- dumpToken - output.token -------');
  reset (tokenSharedPtr^.tokenFile, 'output.token');
  getNextToken;

  writeln (' Base  Line Left Right   pos index Id Token');
  repeat
    tokenCount := tokenCount + 1;
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
                      printReal (realvalue);
                      end;
        dblrealconst: begin
                      write ('DBLREALCONST value: ');
                      printReal (realvalue);
                      end;
        charconst:    write ('CHARCONST value:', intvalue:1);
        stringconst:  write ('STRINGCONST pos:', pos:1, ' len:',len:1);
        ident:        write ('IDENT key:', key:1);
        otherwise     write ('unknownToken - ', ord(token):1);
        end;
      {>>>}
      writeln;
      end;
    getNextToken;
  until (tokenSharedPtr^.nexttoken.token = eofsym) or eof (tokenSharedPtr^.tokenFile);

  with tokenSharedPtr^.nexttoken do
    writeln (baseline:5, line:5, ' ', left:4, ' ', right:4, filePos:5, fileIndex:5, ord(token):3, ' EOF');
  close (tokenSharedPtr^.tokenFile);

  writeln ('------- end of dumpToken count:', tokenCount: 1, '-------');
  close (output);
end.
