{ list.pas }
{<<<}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from
  Oregon Software does not apply to user-modified programs.  All copies
  of this program must display this notice and all copyright notices.


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Listing Printer

 Last modified by KRIS on 21-Nov-1990 15:34:50

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}
{>>>}
{$nomain}
{<<<  includes}
%include 'common.def';
%include 'main.def';
%include 'list.def'
{>>>}

procedure list;

var
  sharedPtr: sharedPtrType;
  listFile: text;

  {<<<}
  procedure listing (var listFile: text);
  {<<<}
  const
    leftmargin = 11; { keep this odd }
    pagelimit = 54;

    tabch = 9; {tab character}
    formfeed = 12; {formfeed character}
    tabspace = 8; {the number of spaces a tab input becomes}
  {>>>}
  {<<<}
  type
    sourcestates = (normal, skippingstring, skippingquote, skippingstar,
                    skippingcomment, builddirective, skippingblanks,
                    buildfilename, buildquotedfilename, skippingfilequote);

    alfa = packed array [1..10] of char; {for directives}

    save_rec =
      record
        line: integer;
        ch: char;
        endofline: boolean;
        filenameLength: FilenameIndex;
        filename: FilenameBuf;
        fileptr: fileRememberPtr;
      end;
  {>>>}
  {<<<}
  var
    nextch: char; {next character to be scanned}

    sourcestate: sourcestates;
    directive: alfa;
    buildindex: integer;

    uniqueerrs: packed array [warning] of boolean {too big for a set!} ;
    e: warning; {for initializing uniqueerrs array}

    pageline, linecount, pagecount, physicalpage, nextErrLine : integer;
    nexterror: integer;
    totallines: integer;

    ch, commentch: char;
    endofline, endofinput: boolean;
    i: integer;

    topofpage, endofpage, skipmode: boolean;
    pagenextline: boolean; { eject next line }

    lineswitherrors: integer;
    errorsreported: integer; {counts errors actually showing in listing}

    hour, minute: integer;
    ampm: char;
    todaystring: packed array [1..9] of char;

    save: array [1..sourcedepth] of save_rec;

    buffer: packed array [1..180] of char;
    bufferCount: integer;

    curfileptr: fileRememberPtr;  { used to step through input files }
    thisfileptr: fileRememberPtr; { the file that is open }
    lastfileptr: fileRememberPtr; { the last file name printed in listing }
  {>>>}

    {<<<}
    function digits (i: integer): integer;

    var
      j: integer;

    begin
      j := 1;
      while i >= 10 do
        begin
        j := j + 1;
        i := i div 10;
        end;
      digits := j;
    end;
    {>>>}
    {<<<}
    procedure dateTime;

    var
      sec, day, month, year: integer; {time buffer}

    begin
      TimeStamp(day, month, year, hour, minute, sec);
      if hour >= 12 then
        ampm := 'P'
      else
        ampm := 'A';

      case month of
        1: todaystring := '  -Jan-  ';
        2: todaystring := '  -Feb-  ';
        3: todaystring := '  -Mar-  ';
        4: todaystring := '  -Apr-  ';
        5: todaystring := '  -May-  ';
        6: todaystring := '  -Jun-  ';
        7: todaystring := '  -Jul-  ';
        8: todaystring := '  -Aug-  ';
        9: todaystring := '  -Sep-  ';
        10: todaystring := '  -Oct-  ';
        11: todaystring := '  -Nov-  ';
        12: todaystring := '  -Dec-  ';
        end {case month} ;

      if day > 9 then
        todaystring[1] := chr(ord('0') + day div 10);
      todaystring[2] := chr(ord('0') + day mod 10);

      year := year mod 100;

      todaystring[8] := chr(ord('0') + year div 10);
      todaystring[9] := chr(ord('0') + year mod 10);
    end;
    {>>>}

    {<<<}
    procedure changeSource;
    { Change source[sourcelevel] to the next remembered file. }

    var
      i: FilenameIndex;

      {<<<}
      procedure seekStringFile (n: integer {byte to access} );
      { Do the equivalent of a "seek" on the string file.
        This sets the file and "nextstringfile" to access byte "n" of the stringfile
      }
      var
        newblock: 1..maxstringblks; { block to which seeking }

      begin
        newblock := n div (diskbufsize + 1) + 1;
        if newblock <> sharedPtr^.curstringblock then
          begin
          sharedPtr^.curstringblock := newblock;
          sharedPtr^.stringblkptr := sharedPtr^.stringblkptrtbl[newblock];
          if sharedPtr^.stringblkptr = nil then
            begin
            write ('unexpected end of stringtable ');
            abort (inconsistent);
            end;
          end;

        sharedPtr^.nextstringfile := n mod (diskbufsize + 1);
      end;
      {>>>}
      {<<<}
      procedure getstringfile;
      { Move the stringfile buffer pointer to the next entry. }

      begin
        if sharedPtr^.nextstringfile >= diskbufsize then
          begin
          sharedPtr^.curstringblock := sharedPtr^.curstringblock + 1;
          sharedPtr^.nextstringfile := 0;
          sharedPtr^.stringblkptr := sharedPtr^.stringblkptrtbl[sharedPtr^.curstringblock];
          if sharedPtr^.stringblkptr = nil then
            begin
            write ('unexpected end of stringtable ');
            abort (inconsistent);
            end;
          end
        else
          sharedPtr^.nextstringfile := sharedPtr^.nextstringfile + 1;
      end;
      {>>>}

    begin
      sharedPtr^.filenameLength := 0;
      if curfileptr <> nil then
        begin
        seekStringFile (sharedPtr^.stringfilecount + curfileptr^.offset - 1);
        while sharedPtr^.stringblkptr^[sharedPtr^.nextstringfile] <> 0 do
          begin
          sharedPtr^.filenameLength := sharedPtr^.filenameLength + 1;
          sharedPtr^.filename[sharedPtr^.filenameLength] := chr(sharedPtr^.stringblkptr^[sharedPtr^.nextstringfile]);
          getstringfile;
          end;

        thisfileptr := curfileptr;
        curfileptr := curfileptr^.next;
        end;

      for i := sharedPtr^.filenameLength + 1 to filenamelen do
        sharedPtr^.filename[i] := ' ';
    end;
    {>>>}
    {<<<}
    procedure startInclude;

    begin
      if sharedPtr^.sourcelevel < sourcedepth then
        begin
        sharedPtr^.sourcelevel := sharedPtr^.sourcelevel + 1;
        save[sharedPtr^.sourcelevel].line := linecount;
        save[sharedPtr^.sourcelevel].ch := nextch;
        save[sharedPtr^.sourcelevel].endofline := endofline;

        changeSource;

        save[sharedPtr^.sourcelevel].filenameLength := sharedPtr^.filenameLength;
        save[sharedPtr^.sourcelevel].filename := sharedPtr^.filename;
        save[sharedPtr^.sourcelevel].fileptr := thisfileptr;

        openSource;

        nextch := ' ';
        endofline := true;
        sourcestate := normal;
        end;
    end;
    {>>>}

    {<<<}
    procedure listLine;
    { Dump out all the characters in the buffer. }

    begin
      writeln (listFile, buffer: bufferCount);
      bufferCount := 0;
    end;
    {>>>}
    {<<<}
    procedure listChr (c: char);
    { Copy a character to the output buffer. }

    begin
      bufferCount := bufferCount + 1;
      buffer[bufferCount] := c;
    end;
    {>>>}
    {<<<}
    procedure listStr (s: packed array [low..high: integer] of char);
    { Copy a string to the output buffer. }

    var
      i: integer;

    begin
      for i := 1 to high do
        if s[i] <> chr(0) then
          begin
          bufferCount := bufferCount + 1;
          buffer[bufferCount] := s[i];
          end;
    end;
    {>>>}
    {<<<}
    procedure listStrL (s: packed array [low..high: integer] of char; n: integer);
    { Copy a string of n bytes to the output buffer. }

    var
      i: integer;

    begin
      for i := 1 to n - high do
        begin
        bufferCount := bufferCount + 1;
        buffer[bufferCount] := ' ';
        end;

      for i := 1 to min(n, high) do
        if s[i] <> chr(0) then
          begin
          bufferCount := bufferCount + 1;
          buffer[bufferCount] := s[i];
          end;
    end;
    {>>>}
    {<<<}
    procedure listInt (v: integer; n: integer);
    { Copy an integer (v) of width n to the output buffer. }

    var
      int_bufptr: 0..9;
      int_buffer: packed array [1..20] of char;
      u: unsignedint;
      s: packed array [1..20] of char;
      ctr: integer;
      i: integer;

    begin
      int_bufptr := 0;
      if v < 0 then listChr('-');
      u := abs(v);

      repeat
        int_bufptr := int_bufptr + 1;
        int_buffer[int_bufptr] := chr(u mod 10 + ord('0'));
        u := u div 10;
      until u = 0;

      ctr := 0;

      repeat
        ctr := ctr + 1;
        s[ctr] := int_buffer[int_bufptr];
        int_bufptr := int_bufptr - 1;
      until int_bufptr = 0;

      for i := 1 to n - ctr do
        begin
        bufferCount := bufferCount + 1;
        buffer[bufferCount] := ' ';
        end;

      for i := 1 to ctr do
        if s[i] <> chr(0) then
          begin
          bufferCount := bufferCount + 1;
          buffer[bufferCount] := s[i];
          end;
    end;
    {>>>}
    {<<<}
    procedure tab (tabfrom, tabto: integer);

    var
      i: integer;

    begin
      for i := tabfrom to tabto - 1 do
        listChr(' ');
    end;
    {>>>}

    {<<<}
    procedure pagelisting;

    begin
      if not topofpage then
        begin
        if bufferCount > 0 then
          listLine;
        page (listFile);
        end;

      pageline := 1;

      topofpage := true;
      endofpage := false;
    end;
    {>>>}
    {<<<}
    procedure printTrailer;
    { Only 3 lines are printed if errors are going to terminal }

    begin
      if sharedPtr^.forceList then
        pageline := pageline + 3
      else
        pageline := pageline + 6;

      if pageline > pagelimit then
        pagelisting;

      { Don't print invocation line if errors are going to terminal }
      if not sharedPtr^.forceList then
        begin
        listLine;
        listStr ('command line:');
        listLine;

        listStrL (sharedPtr^.cmdline, sharedPtr^.cmdlength); { Print user's command line }
        listLine;
        end;
      listLine;

      if lineswitherrors = 0 then
        begin
        listStr (' *** No lines with errors detected ***');
        listLine;
        end

      else if lineswitherrors = 1 then
        begin
        listStr(' *** There was 1 line with errors detected ***');
        listLine;
        end

      else
        begin
        listStr (' *** There were ');
        listInt (lineswitherrors, 1);
        listStr (' lines with errors detected ***');
        listLine;
        end;

    end;
    {>>>}

    {<<<}
    procedure getch;

    var
      i: integer;

      {<<<}
      procedure mapstate;

      var
        i: FilenameIndex;

      begin
        case sourcestate of
          {<<<}
          normal:
            case ch of
              '(':
                if nextch = '*' then
                  begin
                  sourcestate := skippingstar;
                  commentch := ')';
                  end;

              '/':
                if nextch = '*' then
                  begin
                  sourcestate := skippingstar;
                  commentch := '/';
                  end;

              '{':
                begin
                sourcestate := skippingcomment;
                commentch := ')';
                end;

              '''': sourcestate := skippingstring;

              '%':
                begin
                sourcestate := builddirective;
                buildindex := 0;
                directive := '          ';
                end;

              otherwise;
              end;
          {>>>}
          {<<<}
          builddirective:
            if ch in [chr(tabch), ' ', ';'] then
              if directive = 'INCLUDE   ' then
                sourcestate := skippingblanks
              else if directive = 'PAGE      ' then
                begin
                pagenextline := true;
                sourcestate := normal;
                end
              else
                sourcestate := normal

            else if buildindex < 10 then
              begin
              buildindex := buildindex + 1;
              if ch in ['a'..'z'] then
                directive[buildindex] := chr(ord(ch) - (ord('a') - ord('A')))
              else
                directive[buildindex] := ch;
              end;
          {>>>}
          {<<<}
          buildfilename:
            if ch in [' ', ';'] then
              startInclude

            else if sharedPtr^.filenameLength < filenamelen then
              begin
              sharedPtr^.filenameLength := sharedPtr^.filenameLength + 1;
              sharedPtr^.filename[sharedPtr^.filenameLength] := ch;
              end;
          {>>>}
          {<<<}
          buildquotedfilename:
            if ch = '''' then
              sourcestate := skippingfilequote

            else if endofline then
              sourcestate := normal

            else if sharedPtr^.filenameLength < filenamelen then
              begin
              sharedPtr^.filenameLength := sharedPtr^.filenameLength + 1;
              sharedPtr^.filename[sharedPtr^.filenameLength] := ch;
              end;
          {>>>}
          {<<<}
          skippingstring:
            if (ch = '''') then
              if (nextch = '''') then
                sourcestate := skippingquote
              else
                sourcestate := normal;
          {>>>}
          {<<<}
          skippingcomment:
            if (ch = '}') or (ch = '*') and (nextch = commentch) then
              sourcestate := normal;
          {>>>}
          {<<<}
          skippingblanks:
            if (ch <> ' ') and (ch <> chr(tabch)) then
              begin
              for i := 1 to filenamelen do
                sharedPtr^.filename[i] := ' ';

              if ch = '''' then
                begin
                sharedPtr^.filenameLength := 0;
                sourcestate := buildquotedfilename;
                end

              else
                begin
                sharedPtr^.filename[1] := ch;
                sharedPtr^.filenameLength := 1;
                sourcestate := buildfilename;
                end;
              end;
          {>>>}
          skippingfilequote:
            startInclude;
          skippingquote:
            sourcestate := skippingstring;
          skippingstar:
            sourcestate := skippingcomment;
          end;
      end;
      {>>>}

    begin
      ch := nextch;

      endofline := false;
      if eof(sharedPtr^.source[sharedPtr^.sourcelevel]) then
        begin
        if sharedPtr^.sourcelevel > 1 then
          begin
          nextch := save[sharedPtr^.sourcelevel].ch;
          endofline := save[sharedPtr^.sourcelevel].endofline;

          close (sharedPtr^.source[sharedPtr^.sourcelevel]);
          sharedPtr^.sourcelevel := sharedPtr^.sourcelevel - 1;

          { Pop the old filename off stack }
          sharedPtr^.filename := save[sharedPtr^.sourcelevel].filename;
          sharedPtr^.filenameLength := save[sharedPtr^.sourcelevel].filenameLength;
          thisfileptr := save[sharedPtr^.sourcelevel].fileptr;

          for i := 1 to sharedPtr^.sourcelevel do
            save[i].line := save[i].line + linecount - save[sharedPtr^.sourcelevel + 1].line;
          end

        else
          begin
          changeSource;
          save[1].filename := sharedPtr^.filename;
          save[1].filenameLength := sharedPtr^.filenameLength;
          save[1].fileptr := thisfileptr;
          save[1].line := linecount;

          endofinput := true;
          nextch := ' ';
          end;
        end

      else if eoln (sharedPtr^.source[sharedPtr^.sourcelevel]) then
        if nextch = ' ' then
          begin
          readln (sharedPtr^.source[sharedPtr^.sourcelevel]);
          if (sharedPtr^.sourcelevel = 1) or not eof (sharedPtr^.source[sharedPtr^.sourcelevel]) then
            endofline := true;
          end
        else
          nextch := ' '
      else
        read (sharedPtr^.source[sharedPtr^.sourcelevel], nextch);

      mapstate;
    end;
    {>>>}

    {<<<}
    procedure sortErrorTable;

    var
      sorted: boolean;
      temp: errorrecord;
      i, top: integer;

      {<<<}
      function posgtr (i, j: integer): boolean;
      { Determines whether or not the error messages should be swapped  -- preserves time order if errors overlap }

      begin
        with sharedPtr^.errortable[j] do
          if sharedPtr^.errortable[i].errline = errline then
            posgtr := sharedPtr^.errortable[i].errcolumn > errcolumn + digits(ord(err)) + 1
          else
            posgtr := sharedPtr^.errortable[i].errline > errline;
      end;
      {>>>}

    begin
      top := sharedPtr^.lasterror - 1;

      repeat
        sorted := true;
        for i := 1 to top do
          if posgtr (i, i + 1) then
            begin
            temp := sharedPtr^.errortable[i];
            sharedPtr^.errortable[i] := sharedPtr^.errortable[i + 1];
            sharedPtr^.errortable[i + 1] := temp;
            sorted := false;
            top := i - 1;
            end;
      until sorted;
    end;
    {>>>}
    {<<<}
    procedure getNextError;

    begin
      if nexterror < sharedPtr^.lasterror then
        begin
        nexterror := nexterror + 1;
        nextErrLine := sharedPtr^.errortable[nexterror].errline
        end
      else
        nextErrLine := maxint
      end;
    {>>>}
    {<<<}
    procedure processLine;

    var
      stmtno: integer; {statement number for current line (if any)}
      first: boolean; {first printing for this line}
      column, newcolumn: columnindex;
      temppos1, temppos2: integer; {for getpos, because statement file is packed}

      {<<<}
      procedure printLineNum;

        {<<<}
        procedure title;

        var
          i: cmdindex;

        begin
          listStr ('Pascal-2 ');
          listStr (systemtitle);
          listChr (' ');

          listStr (versionstring);
          listChr (' ');

          listStr (todaystring);
          listInt ((hour + 11) mod 12 + 1, 4);
          listChr (':');
          listInt (minute div 10, 1);
          listInt (minute mod 10, 1);
          listChr (' ');

          listChr (ampm);
          listStr ('M ');
          listStr ('    Page ');
          listInt (pagecount, 1);
          listChr ('-');
          listInt (physicalpage, 1);
          listLine;

          listStrL (sharedPtr^.filename, sharedPtr^.filenameLength); { Print current filename }
          lastfileptr := thisfileptr; { don't print this name again }
          listLine;

          listLine;

          totallines := totallines + 4;
          topofpage := false;

          if (sharedPtr^.switcheverplus[debugging] or
              sharedPtr^.switcheverplus[profiling]) and not sharedPtr^.switcheverplus[defineswitch] then
            begin
            listLine;
            listStrL ('Line', leftmargin div 2);
            listStrL ('Stmt', leftmargin div 2);
            listLine;

            pageline := pageline + 2;
            totallines := totallines + 2;
            end;

        end;
        {>>>}

      begin
        if (not topofpage) and (lastfileptr <> thisfileptr) then
          if not sharedPtr^.forceList and (pageline + 4 > pagelimit) then
            begin
            { Start new page }
            pagelisting;
            physicalpage := physicalpage + 1;
            end
          else
            begin {Just print new current filename}
            if not sharedPtr^.forceList then listLine;
            listStrL(sharedPtr^.filename, sharedPtr^.filenameLength);
            listLine;
            listLine;
            lastfileptr := thisfileptr;
            pageline := pageline + 2 + ord(not sharedPtr^.forceList);
            end;

        if topofpage then
          title;

        { if ((sharedPtr^.switcheverplus[debugging] or sharedPtr^.switcheverplus[profiling]) and
           (sharedPtr^.lasterror = 0) and not sharedPtr^.switcheverplus[defineswitch]) then
          msdos, unix, apollo: getpos (listing, temppos1, temppos2);
        }
        if sharedPtr^.sourcelevel > 1 then
          listInt (sharedPtr^.sourcelevel, 1)
        else
          listChr (' ');

        listInt (linecount - save[sharedPtr^.sourcelevel].line, leftmargin div 2 - 1);

        if stmtno <> 0 then
          listInt (stmtno, leftmargin div 2)
        else
          listStrL ('  ', leftmargin div 2);
        listChr (' ');

        first := false;
      end;
      {>>>}
      {<<<}
      function stopConditions: boolean;


        begin {stopconditions}
          stopconditions := endofinput or endofline or endofpage or
                            (column > linelen + 1);
        end {stopconditions} ;
      {>>>}

    begin
      if pagenextline then
        pagelisting;
      pagenextline := false;
      endofpage := false;

      first := true;
      column := 1;
      stmtno := 0;

      {
      if ((sharedPtr^.switcheverplus[debugging] or sharedPtr^.switcheverplus[profiling]) and
          (sharedPtr^.lasterror = 0) and not sharedPtr^.switcheverplus[defineswitch]) then
        (sharedPtr^.linecount, sharedPtr^.stmtno);
      }
      while not stopConditions do
        begin
        getch;
        newcolumn := column;

        while ((ch = chr(tabch)) or (ch = ' ')) and not stopconditions do
          begin
          if (ch = chr(tabch)) and (sourcestate <> skippingstring) then
            newcolumn := ((newcolumn - 1) div tabspace) * tabspace + tabspace + 1
          else
            newcolumn := newcolumn + 1;
          getch;
          end;

        if newcolumn <> column then
          begin
          if first then
            printLineNum;
          tab (column + leftmargin, newcolumn + leftmargin);
          column := newcolumn;
          end;

        if (ch = chr(formfeed)) then
          begin
          if not skipmode then
            endofpage := true;
          end
        else
          begin
          if first then
            printLineNum;
          listChr (ch);
          column := column + 1;
          end;
        end;

      { Flush out a long line -- prevents errors in linecount later. }
      while not (endofinput or endofline or endofpage) do
        getch;

      if endofpage then
        {<<<  end of page}
        begin
        pagecount := pagecount + 1;
        physicalpage := 1;
        pagelisting;
        if ch = chr(formfeed) then
          processLine;
        end
        {>>>}
      else
        {<<<  print line}
        begin
        if first and not endofinput then
          printLineNum;
        listLine;

        {getpos(listing, temppos1, temppos2); }
        totallines := totallines + 1;
        if (pageline > pagelimit) then
          begin
          physicalpage := physicalpage + 1;
          pagelisting;
          end;
        getch;

        linecount := linecount + 1;
        pageline := pageline + 1;
        end;
        {>>>}
    end;
    {>>>}
    {<<<}
    procedure processErrorLine;

    var
      firsterror, lasterror, lastprinted: errorindex;
      errorlines: integer;

      {<<<}
      procedure listErrors;

      var
        i, size: integer;
        linepos: 1..linelen;

      begin
        linepos := 1;
        tab (1, leftmargin + 1);
        for i := firsterror to lasterror do
          with sharedPtr^.errortable[i] do
            begin
            size := digits(ord(err));
            if (errcolumn >= linepos) and (errcolumn + size < linelen) then
              begin
              tab (linepos + leftmargin, errcolumn + leftmargin);
              listChr('^');
              listInt (ord(err), size);
              linepos := errcolumn + size + 1;
              end;
            end;

        listLine;
        totallines := totallines + 1;

        for i := firsterror to lasterror do
          with sharedPtr^.errortable[i] do
            if uniqueerrs[err] then
              begin
              uniqueerrs[err] := false;
              listStr ('*** ');
              listInt (ord(err), 2);
              listStr (': ');
              listOneError (err);
              listLine;
              totallines := totallines + 1;
              errorsreported := errorsreported + 1;
              end;

        listLine;
        totallines := totallines + 1;
      end;
      {>>>}
      {<<<}
      procedure listBriefErrors;

      var
        i: integer;

      begin
        for i := firsterror to lasterror do
          with sharedPtr^.errortable[i] do
            if uniqueerrs[err] then
              begin

              uniqueerrs[err] := false;
              listStrL(sharedPtr^.filename, sharedPtr^.filenameLength);
              listChr('(');
              listInt(max(1, linecount - save[sharedPtr^.sourcelevel].line), 1);
              listStr(') : ');

              listOneError(err);
              listLine;

              totallines := totallines + 1;
              errorsreported := errorsreported + 1;
              end;
      end;
      {>>>}

    begin
      lineswitherrors := lineswitherrors + 1;

      firsterror := nexterror;
      lasterror := nexterror;
      lastprinted := nexterror;
      errorlines := 1;

      uniqueerrs[sharedPtr^.errortable[firsterror].err] := true;
      getNextError;

      while nextErrLine = linecount do
        begin
        lasterror := nexterror;
        with sharedPtr^.errortable[lastprinted] do
          if sharedPtr^.errortable[nexterror].errcolumn > digits(ord(err)) + errcolumn then
            begin
            uniqueerrs[sharedPtr^.errortable[nexterror].err] := true;
            errorlines := errorlines + 1;
            lastprinted := nexterror;
            end;
        getNextError;
        end;

      if pageline + errorlines + 2 > pagelimit then
        begin
        pagelisting;
        physicalpage := physicalpage + 1;
        end
      else
        pageline := pageline + errorlines + 2;

      if sharedPtr^.switcheverplus[editlist] then
        listBriefErrors
      else
        begin
        processLine;
        listErrors;
        end;
    end;
    {>>>}
    {<<<}
    procedure listOneError (err: warning);
    { Translate error scalar type to meaningful message on listing file. }

    begin
      case err of
        linetoolong: listStr('Line too long');
        badchar: listStr('Illegal character');
        missingdigits: listStr('Need at least 1 digit after ''.'' or ''E''');
        octalconst: listStr('Octal constants are not standard Pascal');
        nondecimalconst: listStr('Non-decimal integers are not standard Pascal');
        toomanyerrors: listStr('Too many errors!');
        badoctal: listStr('Octal constant contains an illegal digit');
        badradix: listStr('Non-decimal integer base must lie in range 2..16');
        {<<<}
        badinteger:
          begin
          listStr('Integers must lie in range ');
          listInt( - sharedPtr^.targetmaxint - 1, 1);
          listStr('..');
          listInt(sharedPtr^.targetmaxint, 1);
          end;
        {>>>}
        {<<<}
        badexpon:
          begin
          listStr('Exponent must lie in range ');
          if sharedPtr^.switcheverplus[doublereals] then
            begin
            listInt(mindoubleexpon, 1);
            listStr('..');
            listInt(maxdoubleexpon, 1);
            end
          else
            begin
            listInt(minexpon, 1);
            listStr('..');
            listInt(maxexpon, 1);
            end;
          end;
        {>>>}
        zerostring: listStr('String of length zero');
        {<<<}
        levelerr:
          begin
          listStr('Only ');
          listInt(maxlevel, 1);
          listStr(' levels of nesting allowed');
          end;
        {>>>}
        doteoferr: listStr('Use ''.'' after main program body');
        extraenderr: listStr('Extra END following block -- Check BEGIN ... END pairing');
        extraprocerr: listStr('Extra procedures found after main program body');
        extrastmterr: listStr('Extra statements found after end of program');
        garbageerr: listStr('Nonsense discovered after program end');
        blockstarterr: listStr('Block must begin with LABEL,CONST,TYPE,VAR,PROCEDURE,FUNCTION, or BEGIN');
        scrambledblkerr: listStr('Block declarations are incorrectly ordered');
        badlabelnest: listStr('Label is target of illegal GOTO');
        nosemierr: listStr('Use '';'' to separate statements');
        nobeginerr: listStr('BEGIN expected');
        blockenderr: listStr('Block ended incorrectly');
        noenderr: listStr('END expected');
        stmtenderr: listStr('Statement ended incorrectly');
        nountilerr: listStr('UNTIL expected');
        badelseerr: listStr('Unexpected ELSE clause -- Check preceding IF for extra '';''');
        nothenerr: listStr('THEN expected');
        nocommaerr: listStr(''','' expected');
        nocolonerr: listStr(''':'' expected');
        nooferr: listStr('OF expected');
        caselabelerr: listStr('Bad CASE label');
        caseelseerr: listStr('OTHERWISE/ELSE clause in CASE not allowed');
        nodoerr: listStr('DO expected');
        nobecomeserr: listStr(''':='' expected');
        nodowntoerr: listStr('TO or DOWNTO expected');
        nofilevar: listStr('File variable expected');
        novarerr: listStr('Identifier expected');
        badlabelerr: listStr('Label must be unsigned integer constant');
        norparerr: listStr(''')'' expected');
        badcolonerr: listStr('Procedures cannot be followed by type definition');
        badparamerr: listStr('Bad parameter element');
        notypenameerr: listStr('Type name expected');
        nosemiprocerr: listStr(''';'' expected after procedure body');
        nofuncass: listStr('Function identifier is never assigned a value');
        badexprerr: listStr('Badly formed expression');
        nooperr: listStr('Binary operator expected');
        nooprnderr: listStr('Operand expected');
        badindexerr: listStr(''']'' or '','' must follow index expression');
        norbrackerr: listStr(''']'' expected');
        badrparerr: listStr('Unexpected '')'' -- Check for matching parenthesis');
        noeqlerr: listStr('''='' expected');
        badconsterr: listStr('Bad constant');
        nosemiheaderr: listStr('Use '';'' to separate declarations');
        baddeclerr: listStr('Declaration terminated incorrectly');
        badtypesyntax: listStr('Bad type syntax');
        nolabelerr: listStr('Integer label expected');
        nolbrackerr: listStr('''['' expected');
        nodotdoterr: listStr('''..'' expected');
        nolparerr: listStr('''('' expected');
        {<<<}
        proctablefull:
          begin
          listStr('Too many procedures (only ');
          listInt(proctablesize, 1);
          listStr(' allowed)');
          end;
        {>>>}
        {<<<}
        undeftablefull:
          begin
          listStr('Too many forward declarations (only ');
          listInt(undeftablesize, 1);
          listStr(' allowed)');
          end;
        {>>>}
        {<<<}
        tablefull:
          begin
          listStr('Too many identifiers (only ');
          listInt(hashtablesize, 1);
          listStr(' allowed)');
          end;
        {>>>}
        stringtableoverflow: listStr('Too many strings or identifiers');
        baddirective: listStr('Unknown directive');
        {<<<}
        deepinclude:
          begin
          listStr('Too many nested INCLUDE directives (only ');
          listInt(sourcedepth - 1, 1);
          listStr(' allowed)');
          end;
        {>>>}
        duplicateident: listStr( 'Identifier cannot be redefined or defined after use at this level');
        undefidenterr: listStr('Undefined identifier');
        indexerror: listStr('Array subscript out of range');
        overflow: listStr('Integer overflow or division by zero');
        bigarrayerr: listStr('Array exceeds addressable memory');
        rangeerror: listStr('Assignment value out of range');
        badsubrange: listStr('Illegal subrange');
        badindex: listStr('Index must be non-real scalar type');
        badsetbase: listStr('Sets must be non-real scalar type');
        badsetexpression: listStr('Set is constructed of incompatible types');
        badcasetyp: listStr('Case label must be non-real scalar type');
        badcaselab: listStr('Case label type does not match tag field type');
        duplicatetag: listStr('Tag identifier already used in this record');
        duplabeldef: listStr('Label cannot be redefined at this level');
        labnotpredef: listStr('Label must be declared in LABEL declaration');
        badlabeldef: listStr('Label defined twice');
        badtagerr: listStr('Tag does not appear in variant record label list');
        labelundef: listStr('Declared labels must be defined in procedure body');
        fwdundef: listStr('Forward procedure/function body is never defined');
        typeundef: listStr('Forward type reference is never resolved');
        dupfwdparam: listStr( 'Parameter list cannot be duplicated in forward-declared procedure/function body');
        dupfwdresult: listStr( 'Function result type cannot be duplicated in forward-declared function body');
        dupforward: listStr( 'This procedure/function name has been previously declared forward' );
        fwdprocfuncerr: listStr('This function was declared as a forward procedure');
        fwdfuncprocerr: listStr('This procedure was declared as a forward function');
        badxdef: listStr( 'External procedures/functions must be defined at outermost level' );
        recordexpected: listStr('Variable of type record expected');
        arrayexpected: listStr('Variable of type array expected');
        ptrexpected: listStr('File variable or pointer variable expected');
        badfunctionarg: listStr( 'Function cannot be applied to an operand of this type');
        illegalformat: listStr( 'This parameter cannot be followed by a format expression');
        badformat: listStr('Format expression must be of type integer');
        badreadtype: listStr('Variables of this type are not allowed in READ');
        noreadarg: listStr('Need at least one variable to READ');
        badwritearg: listStr('Variables of this type are not allowed in WRITE');
        nostringerr: listStr('Packed array [1..n] of characters expected');
        filenameerr: listStr('File names in RESET/REWRITE are non-standard');
        noptrvar: listStr('Pointer variable expected');
        nofieldtype: listStr('Field variable expected for NEW');
        badnewlabel: listStr('Variant label is undefined');
        nowritearg: listStr('Need at least one value to WRITE');
        toomanyargs: listStr('Too many actual parameters');
        toofewargs: listStr('Too few actual parameters');
        paramtypeerr: listStr( 'Actual parameter type doesn''t match formal parameter type');
        booleanexpected: listStr('Boolean value expected');
        badarithtype: listStr('Operator cannot be applied to these operand types');
        signedseterr: listStr( 'Unary ''+'' or ''-'' cannot be applied to set operands');
        badreloprnds: listStr( 'Illegal comparison of record, array, file, or pointer values' );
        badrealtoint: listStr( 'Can''t assign a real value to an integer variable (use TRUNC or ROUND)' );
        baddbltoreal: listStr( 'Can''t assign a double value to a real variable (use SNGL)' );
        typesincomp: listStr('Operands are of differing or incompatible type');
        compilerwritererr: listStr( 'Compiler writer error -- please contact Oregon Software at (503) 245-2202' );
        nostrictinclusion: listStr('No strict inclusion of sets allowed');
        badinoprnds: listStr('Bad IN operands');
        badforvar: listStr( 'FOR-loop control variable must be declared at this level');
        unsupportedforvardecl: listStr( 'FOR-loop control variable declared as OWN, USE, DEFINE, SHARED, OR ORIGIN');
        badfortype: listStr( 'FOR-loop control variable can only be a simple non-real scalar variable' );
        badforlimit: listStr( 'Expression type is incompatible with FOR index type');
        badcasetype: listStr( 'CASE selection expression must be a non-real scalar type');
        badcaselabeltype: listStr( 'CASE label does not match selection expression type');
        indexincomp: listStr( 'Index expression type does not match array declaration');
        badprocparam: listStr('Procedure name expected');
        badfuncparam: listStr('Function name expected');
        varparamerr: listStr( 'VAR parameters cannot be passed an expression, packed field or variant tag');
        badassignment: listStr( 'Assignment operands are of differing or incompatible types');
        cantpack: listStr('Can''t pack unstructured or named type');
        wantvarname: listStr('Variable name expected');
        nofilefile: listStr('File cannot contain a file component');
        dupcaselabel: listStr('Case label defined twice');
        badfunctype: listStr('Function result must be of scalar or pointer type');
        badfuncassign: listStr('Illegal function assignment');
        missingforindex: listStr('Index variable missing in this FOR statement');
        modifiedfor: listStr( 'Reassignment of FOR-loop control variable not allowed');
        badprocfunc: listStr('Only functions can be called from expressions');
        badassign: listStr('Assignment to constants not allowed');
        norecordident: listStr('Record identifier expected');
        unassigned: listStr('Must assign value before using variable');
        badorigin: listStr('Bad ORIGIN value');
        novaluefile: listStr('Files must be passed as VAR parameters');
        dontassignfile: listStr('Assignment of file variables not allowed');
        longstring: listStr('String constants may not include line separator');
        bigsetbase: listStr('Set types must have a base in the range 0..255');
        nottextfile: listStr('Readln, writeln, eoln, and page must be applied to text file');
        obsoletecomments: listStr('Non-standard comment form, please use "{" or "(*"');
        typenotallowed: listStr('A type identifier is not allowed here');
        progexpected: listStr('PROGRAM heading expected');
        badmodop: listStr('The divisor of a mod must be greater than zero');
        badpackconform: listStr( 'Packed conformant array parameters cannot be nested');
        confinconsistent: listStr( 'All parameters in a single parameter section must have the same type.' );
        badconfactual: listStr( 'Actual parameter cannot be used with this conformant array parameter.' );
        bigrecorderr: listStr('Record too large');
        bigblockerr: listStr('Data declarations for this block are too large');
        {<<<}
        biglabelerr:
          begin
          listStr('Label must lie in range 0..');
          listInt(maxstandardlabel, 1);
          end;
        {>>>}
        badnumber: listStr( 'Blank characters must separate identifiers from numeric constants');
        badfornestref: listStr('Nested procedure modifies index variable');
        badcasetags: listStr('Variant tags do not exactly match range of tag type');
        nameundef: listStr('PROGRAM parameter is never defined');
        filenotdeclared: listStr('External file must be declared in PROGRAM statement');
        inputnotdeclared: listStr('Standard file "input" must be declared by PROGRAM statement');
        outputnotdeclared: listStr( 'Standard file "output" must be declared by PROGRAM statement');
        novarianttag: listStr( 'Variant record case selector may not be passed as a VAR parameter');
        notlevel0: listStr('Conformant arrays are not Level 0');
        toomanyelements: listStr('Too many array elements');
        eofincomment: listStr('End of file encountered in a comment');
        baddouble: listStr('Embedded DOUBLE switch is illegal after first token');
        manyscopes: listStr( 'Too many records, or forward, external or nonpascal procedures/functions' );
        baduniv: listStr('UNIV may only be used with VAR parameters');
        badstringindex: listStr('STRING limit must be an integer in the range 1..255');
        stringoverflowerr: listStr('STRING exceeds allocated size');
        bodyfounderr: listStr( 'Procedure or function bodies not allowed with "define" option.');
        manyenviron: listStr('Only one environment directive allowed.');
        badenviron: listStr( 'Environment directive must precede all declarations.');
        badoptions: listStr( 'Environment file options inconsistent with currently defined options.');
        baddefine: listStr( 'This type of declaration not allowed with "define" option.');
        badcase: listStr('Embedded case switch is illegal after first token');
        toomanyextvars: listStr('Too many external variables');
        badsharedvar: listStr('Shared variable declaration error');
        badusedefinevar: listStr('Use and/or define not allowed here');
        badcvtfunc: listStr('SNGL and DBL are illegal with DOUBLE switch');
        badinterruptproc: listStr('Illegal interrupt procedure');
        badmultidef: listStr('Improper redefinition of use/define variable');
        {<<<}
        otherwise
          begin
          listStr('??? UNKNOWN ERROR REPORTED ???');
          end;
        {>>>}
        end {case err} ;
    end;
    {>>>}
    {<<<}
    procedure skipWithErrors (limit: integer);

    var
      pseudopageline: integer;

      {<<<}
      procedure skip (limit: integer);

      var
        column: columnindex;
        endofpage: boolean;

      begin
        while linecount < limit do
          begin
          column := 1;
          endofpage := (pseudopageline > pagelimit);

          while not (endofinput or endofline or endofpage or (pseudopageline > pagelimit)) do
            begin
            getch;
            if ch = chr(tabch) then
              column := (column div tabspace) * tabspace + tabspace
            else if ch = chr(formfeed) then
              endofpage := true
            else
              column := column + 1;
            end;

          if endofpage then
            begin
            if (pseudopageline > pagelimit) then
              physicalpage := physicalpage + 1
            else
              begin
              physicalpage := 1;
              pagecount := pagecount + 1;
              end;
            pseudopageline := 1;
            end

          else
            begin
            getch;
            linecount := linecount + 1;
            pseudopageline := pseudopageline + 1;
            end;
          end;
      end;
      {>>>}

    begin
      skipmode := true;
      pseudopageline := pageline;
      while nextErrLine < limit do
        begin
        skip (nextErrLine);
        processErrorLine;
        end;

      skip (limit)
    end;
    {>>>}
    {<<<}
    procedure listWithErrors (limit: integer);

      {<<<}
      procedure listSource (limit: integer);

      begin
        while linecount < limit do
          processLine;
      end;
      {>>>}

    begin
      skipmode := false;

      while nextErrLine < limit do
        begin
        listsource (nextErrLine);
        processErrorLine;
        end;

      listSource (limit)
    end;
    {>>>}

  begin
    bufferCount := 0;

    save[1].line := 0;
    sharedPtr^.sourcelevel := 1;
    sharedPtr^.curstringblock := - 1;

    { The following two lines replace the file name found on the command line,
      with the file name saved by SCAN at the beginning of the compilation.
      We then use OPENS to get to the file by name. }
    curfileptr := sharedPtr^.fileRememberList;
    changeSource;

    lastfileptr := nil;
    save[1].filename := sharedPtr^.filename;
    save[1].filenameLength := sharedPtr^.filenameLength;
    save[1].fileptr := thisfileptr;
    openSource;

    sourcestate := normal;
    topofpage := true;
    endofpage := false;
    pagenextline := false;
    endofline := false;
    endofinput := false;
    linecount := 1;
    pageline := 1;
    pagecount := 1;
    physicalpage := 1;
    totallines := 1;
    nexterror := 0;
    lineswitherrors := 0;
    nextch := ' ';
    getch;

    errorsreported := 0;
    for e := firstwarning to lastwarning do
      uniqueerrs[e] := false;

    dateTime;
    sortErrorTable;
    getNextError;

    i := 1;
    while i <= sharedPtr^.lastList do
      begin
      with sharedPtr^.listTable[i] do
        begin
        skipWithErrors (start);
        listWithErrors (start + count);
        end;
      i := i + 1;
      end;

    printTrailer;

    sharedPtr^.lasterror := errorsreported;
  end;
  {>>>}

begin
  sharedPtr := getSharedPtr;

  if sharedPtr^.forceList then
    listing (output)

  else
    begin
    getFileName (sharedPtr^.listname, false, false, sharedPtr^.filename, sharedPtr^.filenameLength);
    rewrite (listFile, 'output.txt');

    { If the last region of the file is nolisted and the list command line option is used,
      this dummy listing line will force a forceList of any errors in the nolisted region.
      This fix causes no harm in the normal case }
    sharedPtr^.lastList := sharedPtr^.lastList + 1;
    sharedPtr^.listTable[sharedPtr^.lastList].start := sharedPtr^.lastLine;
    sharedPtr^.listTable[sharedPtr^.lastList].count := 0;

    listing (listFile);
    close (listFile);
    end;
end;
