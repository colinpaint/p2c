program dumpInter (output);
{ dumpInter.pas - dump intermediate code file }
%include 'common.def';
%include 'inter.def';

var
  interSharedPtr: interSharedPtrType;
  nextintcode: integer;
  outputfilename: packed array [1..255] of char;

  {<<<}
  procedure getintfile;
  { Do the equivalent of a "get" on the intermediate file from analys.
    If the last element of the buffer was just read, a new buffer is
    read.  Otherwise the global index "nextintcode" is incremented.
    The next element is referenced as intfile^[nextintcode].

    This routine is also responsible for setting switchcounters to
    reflect the effect of embedded switches.  It also updates the element
    counts (mlow and mhi) to those for the output file.  This effectively
    groups all switches changed in a procedure to the front of that
    procedure.
  }
  begin
    if nextintcode = diskbufsize then
      begin
      nextintcode := 0;
      if not eof(interSharedPtr^.interFile) then
        get (interSharedPtr^.interFile);
      end
    else
      nextintcode := nextintcode + 1;
  end;
  {>>>}
  {<<<}
  function getintfileint: unsignedint;
  { Returns an integer passed in the intermediate file as a pair of bytes. }

  var
    { This fudges an integer into bytes.  The constant "32" is large enough number to include all probable systems. }
    fudge:
      record
        case boolean of
          true: (int: unsignedint);
          false: (byte: packed array [1..32] of hostfilebyte);
      end;

    j: 1..32;

  begin
    fudge.int := interSharedPtr^.interFile^.block[nextintcode].b;
    if fudge.int = hostfilelim then
      for j := 1 to hostintsize * hostfileunits do
        begin
        getintfile;
        fudge.byte[j] := interSharedPtr^.interFile^.block[nextintcode].b;
        end;

    getintfileint := fudge.int;
  end;
  {>>>}

  {<<<}
  procedure statement;

  var
    i: integer;

    {<<<}
    procedure prints;

    begin
      case interSharedPtr^.interFile^.block[nextintcode].s of
        begblk:    write ('begblk');
        begcase:   write ('begcase');
        begcfor:   write ('begcfor');
        begdata:
          {<<<}
          begin
          writeln ('begdata');

          processDataInits;

          { now that all the data is gone, do the begblk }
          prints;
          end;
          {>>>}
        begelse:   write ('begelse');
        begfor:    write ('begfor');
        begexit:   write ('begexit');
        begif:     write ('begif');
        begloop:   write ('begloop');
        begreturn: write ('begreturn');
        begrpt:    write ('begrpt');
        begswitch: write ('begswitch');
        begwhile:  write ('begwhile');
        begwith:   write ('begwith');

        blksize:   write ('blksize');

        casedef:   write ('casedef');
        caselab:   write ('caselab');
        caselabrange: write ('caselabrange');

        deflab:    write ('deflab');

        endall:    write ('endall');
        endblk:    write ('endblk');
        endcase:   write ('endcase');
        endcaseelt: write ('endcaseelt');
        endcfor:   write ('endcfor');
        endelse:   write ('endelse');
        endfor:    write ('endfor');
        endloop:   write ('endloop');
        endrpt:    write ('endrpt');
        endswitch: write ('endswitch');
        endthen:   write ('endthen');
        endwhile:  write ('endwhile');
        endwith:   write('endwith');

        fordn:     write ('fordn');
        forup:     write ('forup');

        gotolab:   write ('gotolab');

        hiddenstmt: write ('hiddenstmt');

        inits:     write ('inits');

        loopbreak: write ('loopbreak');
        loopcont:  write ('loopcont');

        simple:    write ('simple');

        switchbreak: write ('switchbreak');

        syscall:   write ('syscall');
        otherwise  write ('state ', ord(interSharedPtr^.interFile^.block[nextintcode].s): 3);
        end;
    end;
    {>>>}
    {<<<}
    procedure processDataInits;
    { Initialization data is passed in the intermediate file as a series of
      operators.  Such operators can appear in thd midst of an expression,
      or they can be preceeded by a "begdata" statement and appear outside
      of a statement or even a block body.  They do not cause anything to
      be placed in the tree, but generate pseudo-ops directly }

      {<<<}
      procedure doDataOp;
      { Take a data operator and generate output code. }

      var
        this_op: operator; {current operator}
        len, op1, op2: integer; {operands}

      begin
        this_op := interSharedPtr^.interFile^.block[nextintcode].o;

        if this_op = drealop then
          begin
          write ('drealop ');
          getintfile;
          len := getintfileint;
          write (len:-8);
          end

        else
          begin
          len := 0;
          op1 := 0;
          op2 := 0;

          case this_op of
            daddop, dsubop, dstoreop, dendop:
              {<<<}
              if this_op = daddop then
                write ('daddop')
              else if this_op = dsubop then
                write ('dsubop')
              else if this_op = dendop then
                write ('dendop')
              else
                write ('dstoreop');
              {>>>}
            daddrop:
              {<<<}
              begin
              write ('daddrop ');

              getintfile;
              len := getintfileint;

              getintfile;
              op1 := getintfileint;

              getintfile;
              op2 := getintfileint;

              write (len:1, ' ', op1:1, ' ', op2:1);
              end;
              {>>>}
            dfaddrop, dintop, dstructop:
              {<<<}
              begin
              if this_op = dfaddrop then
                write ('dfaddrop ')
              else if this_op = dintop then
                write ('dintop ')
              else
                write ('dstructop ');

              getintfile;
              len := getintfileint;

              getintfile;
              op1 := getintfileint;

              write (len:1, ' ', op1:1);
              end;
              {>>>}
            dstartop, dfieldop:
              {<<<}
              begin
              if this_op = dstartop then write ('dstartop ') else write ('dfieldop ');

              getintfile;
              op1 := getintfileint;

              getintfile;
              op2 := getintfileint;

              write (op1:1, ' ', op2:1);
              end;
              {>>>}
            dfillop:
              {<<<}
              begin
              write ('dfillop ');

              getintfile;
              len := getintfileint;

              write (len:1);
              end;
              {>>>}
            end;
          end;
      end;
      {>>>}

    begin
      getintfile;
      repeat
        while interSharedPtr^.interFile^.block[nextintcode].o <> endexpr do
          begin
          doDataOp;
          writeln;
          getintfile;
          end;

        writeln ( 'endexpr');

        getintfile; {skip the endexpr}
      until interSharedPtr^.interFile^.block[nextintcode].s <> begdata;
    end;
    {>>>}
    {<<<}
    procedure expression;

      {<<<}
      procedure printo;

      begin
        case interSharedPtr^.interFile^.block[nextintcode].o of
          daddop, daddrop, dfaddrop, dfieldop, dfillop, dintop, drealop,
          dstartop, dstoreop, dstructop, dsubop, dendop: ; {do nothing}

          modeqop: write ('modeqop');
          muleqop: write ('muleqop');
          shiftleqop: write ('shiftleqop');
          shiftreqop: write ('shiftreqop');
          xoreqop: write ('xoreqop');
          preincop: write ('preincop');
          postincop: write ('postincop');
          xorop: write ('xorop');
          shiftrop: write ('shiftrop');
          questop: write ('questop');
          pushret: write ('pushret');
          tempop: write ('tempop');
          addeqop: write ('addeqop');
          addrop: write ('addrop');
          aindxop: write ('aindxop');
          andop: write ('andop');
          andeqop: write ('andeqop');
          oreqop: write ('oreqop');
          castfptrop: write ('castfptrop');
          castintop: write ('castintop');
          castptrop: write ('castptrop');
          castrealop: write ('castrealop');
          chrstrop: write ('chrstrop');
          chrstrop1: write ('chrstrop1');
          commaop: write ('commaop');
          clearnewop: write ('clearnewop');
          arraystrop: write ('arraystrop');
          arraystrop1: write ('arraystrop1');
          bldfmt: write ('bldfmt');
          bldnil: write ('bldnil');
          bldset: write ('bldset');
          call: write ('call');
          callparam: write ('callparam');
          cindxchkop: write ('cindxchkop');
          closerangeop: write ('closerangeop');
          cmoveop: write ('cmoveop');
          congruchkop: write ('congruchkop');
          copystackop: write ('copystackop');
          dbl_to_real: write ('dbl_to_real');
          decop: write ('decop');
          defforindexop: write ('defforindexop');
          defforlitindexop: write ('defforlitindexop');
          definelazyop: write ('definelazyop');
          defunsforindexop: write ('defunsforindexop');
          defunsforlitindexop: write ('defunsforlitindexop');
          diveqop: write ('diveqop');
          divop: write ('divop');
          doubleop: write ('doubleop');
          dummyargop: write ('dummyargop');
          dummyarg2op: write ('dummyarg2op');
          endexpr: write ('endexpr');
          eqlit: write ('eqlit');
          eqop: write ('eqop');
          filebufindrop: write ('filebufindrop');
          float: write ('float');
          float1: write ('float1');
          float_double: write ('float_double');
          fordnchkop: write ('fordnchkop');
          forerrchkop: write ('forerrchkop');
          forindexop: write ('forindexop');
          forupchkop: write ('forupchkop');
          fptrop: write ('fptrop');
          geqlit: write ('geqlit');
          geqop: write ('geqop');
          globalop: write ('globalop');
          ownop: write ('ownop');
          segop: write ('segop');
          extop: write ('extop');
          gtrlit: write ('gtrlit');
          gtrop: write ('gtrop');
          incop: write ('incop');
          indrop: write ('indrop');
          indxchkop: write ('indxchkop');
          indxop: write ('indxop');
          inop: write ('inop');
          intop: write ('intop');
          kwoop: write ('kwoop');
          leqlit: write ('leqlit');
          leqop: write ('leqop');
          levop: write ('levop');
          lit: write ('lit');
          localop: write ('localop');
          loopholeop: write ('loopholeop');
          lsslit: write ('lsslit');
          lssop: write ('lssop');
          minusop: write ('minusop');
          modop: write ('modop');
          movelit: write ('movelit');
          moveop: write ('moveop');
          mulop: write ('mulop');
          negop: write ('negop');
          neqlit: write ('neqlit');
          neqop: write ('neqop');
          newset: write ('newset');
          newunsvarop: write ('newunsvarop');
          newvarop: write ('newvarop');
          notop: write ('notop');
          openarrayop: write ('openarrayop');
          originop: write ('originop');
          orop: write ('orop');
          paindxop: write ('paindxop');
          parmop: write ('parmop');
          pindxop: write ('pindxop');
          plusop: write ('plusop');
          ptrchkop: write ('ptrchkop');
          ptrop: write ('ptrop');
          pushaddr: write ('pushaddr');
          pushstraddr: write ('pushstraddr');
          pushcvalue: write ('pushcvalue');
          pushfinal: write ('pushfinal');
          pushlitvalue: write ('pushlitvalue');
          pushproc: write ('pushproc');
          pushfptr: write ('pushfptr');
          pushvalue: write ('pushvalue');
          quoop: write ('quoop');
          rangechkop: write ('rangechkop');
          rd: write ('rd');
          realop: write ('realop');
          real_to_dbl: write ('real_to_dbl');
          remop: write ('remop');
          reserve: write ('reserve');
          restop: write('restop');
          returnop: write ('returnop');
          saveop: write ('saveop');
          setbinfileop: write ('setbinfileop');
          setelt: write ('setelt');
          setfileaddrop: write ('setfileaddrop');
          setfileop: write ('setfileop');
          setinput: write ('setinput');
          setpair: write ('setpair');
          shiftlop: write ('shiftlop');
          slashop: write ('slashop');
          stddivop: write ('stddivop');
          stdmodop: write ('stdmodop');
          structop: write ('structop');
          subeqop: write ('subeqop');
          switchstack: write ('switch');
          sysfn: write ('sysfn');
          unscall: write ('unscall');
          unscallparam: write ('unscallparam');
          unsvarop: write ('unsvarop');
          varop: write ('varop');
          vindxop: write ('vindxop');
          withop: write ('withop');
          wr: write ('wr');
          otherwise write ('operator ', ord(interSharedPtr^.interFile^.block[nextintcode].o): 3);
          end;

      end;
      {>>>}
      {<<<}
      procedure op;

      var
        realkluge:
          record
            case boolean of
              false: (i: packed array [0..1] of integer);
              true: (r: double);
          end;

        t: unsignedint;
        s: addressrange;
        i: 1..32;

        {<<<}
        procedure printf;

        begin
          case interSharedPtr^.interFile^.block[nextintcode].f of
            arrays: write ('arrays');
            bools: write ('bools');
            chars: write ('chars');
            doubles: write ('doubles');
            fields: write ('fields');
            strings: write ('strings');
            files: write ('files');
            ints: write ('ints');
            none: write ('none');
            fptrs: write ('fptrs');
            ptrs: write ('ptrs');
            reals: write ('reals');
            scalars: write ('scalars');
            sets: write ('sets');
            subranges: write ('subranges');
            variantlabs: write ('variantlabs');
            flexarrays: write ('flexarrays');
            words : write ('words');
            bytes : write ('bytes');
            opaques : write ('opaques');
            procs : write ('procs');
            stringliterals : write ('stringliterals');
            otherwise write ('form ', ord(interSharedPtr^.interFile^.block[nextintcode].f): 3);
            end;
        end;
        {>>>}

      begin
        printo;

        case interSharedPtr^.interFile^.block[nextintcode].o of
          daddop, daddrop, dfaddrop, dfieldop, dfillop, dintop, drealop,
          dstartop, dstoreop, dstructop, dsubop, dendop:
            do_data_op;

          addrop, aindxop, andop, bldfmt, bldset, call, callparam, cindxchkop,
          closerangeop, cmoveop, congruchkop, copystackop, decop, andeqop,
          definelazyop, divop, eqlit, eqop, filebufindrop, float, float1,
          chrstrop, chrstrop1, arraystrop, arraystrop1, fordnchkop, oreqop,
          forerrchkop, forupchkop, geqlit, geqop, gtrlit, gtrop, incop,
          indrop, indxchkop, indxop, inop, leqlit, leqop, loopholeop, lsslit,
          lssop, minusop, movelit, moveop, mulop, negop, neqlit, neqop, notop,
          orop, paindxop, pindxop, plusop, ptrchkop, pushaddr, pushcvalue,
          pushfinal, pushlitvalue, pushproc, pushstraddr, pushvalue, quoop,
          rangechkop, rd, remop, returnop, setbinfileop, setelt, setfileaddrop,
          setfileop, setpair, shiftlop, slashop, stddivop, sysfn, unscall,
          unscallparam, wr, float_double, real_to_dbl, dbl_to_real, dummyargop,
          dummyarg2op, openarrayop, addeqop, subeqop, pushfptr, vindxop,
          parmop, commaop, groupop, compop, castfptrop, castintop, castptrop,
          castrealop, diveqop, modeqop, muleqop,
          shiftleqop, shiftreqop, xoreqop, preincop, postincop, xorop,
          shiftrop, questop, pushret, tempop:
            {<<<}
            begin
            getintfile;
            write (' len:', getintfileint: 1);
            getintfile;
            write (' cost:', getintfileint: 1, ' ');
            getintfile;
            printf;
            end;
            {>>>}
          withop:
            {<<<}
            begin
            getintfile;
            write (' ', getintfileint: 1);
            end;
            {>>>}
          newvarop, newunsvarop, unsvarop, varop:
            {<<<}
            begin
            getintfile;
            write (' len:', getintfileint: 1);
            getintfile;
            write (' lev:', getintfileint: 1);
            getintfile;
            write (' indx:', getintfileint: 1);
            getintfile;
            write (' ownvar:',loophole(boolean, getintfileint));
            end;
            {>>>}
          forindexop:
            {<<<}
            begin
            getintfile;
            write (' depth:', getintfileint: 1);
            end;
            {>>>}
          fptrop:
            {<<<}
            begin
            getintfile;
            write (', proc:', getintfileint: 1);
            end;
            {>>>}
          defforindexop, defforlitindexop, defunsforindexop,
          defunsforlitindexop:
            {<<<}
            begin
            getintfile;
            write (' len:', getintfileint: 1);
            getintfile;
            write (' lev:', getintfileint: 1);
            end;
            {>>>}
          reserve:
            {<<<}
            begin
            getintfile;
            write (' len:', getintfileint: 1);
            end;
            {>>>}
          structop, levop:
            {<<<}
            begin
            getintfile;
            write (' ', getintfileint: 1, ',');
            getintfile;
            write (' ', getintfileint: 1);
            end;
            {>>>}
          realop, doubleop:
            {<<<}
            begin
            s := 0;
            i := 1;
            while s < size(realarray) do
              begin
              getintfile;
              t := getintfileint;
              write (' ', t mod 16#10000: -4, ' ', t div 16#10000: -4);
              i := i + 1;
              s := s + size(integer);
              end;
            end;
            {>>>}
          intop, ptrop, lit, originop, segop, extop:
            {<<<}
            begin
            getintfile;
            write (' ', getintfileint: 1);
            end;
            {>>>}
          otherwise;
          end;

        writeln;
        getintfile;
      end;
      {>>>}

    begin
      while interSharedPtr^.interFile^.block[nextintcode].o <> endexpr do
        op;
      printo;
    end;
    {>>>}

  begin
    prints;
    case interSharedPtr^.interFile^.block[nextintcode].s of
      forup, fordn:
        {<<<}
        begin
        getintfile;
        write (' by:', getintfileint: 1);
        end;
        {>>>}
      syscall:
        {<<<}
        begin
        getintfile;
        write (' srcindex: ',getintfileint:1);
        getintfile;
        write (' line:', getintfileint: 1);
        getintfile;
        writeln(' n:', getintfileint: 1);
        getintfile;
        expression;
        end;
        {>>>}
      begblk:
        {<<<}
        begin
        getintfile;
        write (' line:', getintfileint: 1);

        getintfile;
        write (' ref:', getintfileint: 1);

        getintfile;
        write (' ps:', getintfileint: 1);

        getintfile;
        write (' bs:', getintfileint: 1);

        getintfile;
        write (' baseline:', getintfileint: 1);

        end;
        {>>>}
      begreturn, begcase, begfor, begif, begwhile, begwith, simple:
        {<<<}
        begin
        getintfile;
        write (' srcindex: ',getintfileint:1);
        getintfile;
        writeln(' line:', getintfileint: 1);
        getintfile;
        expression
        end;
        {>>>}
      begloop, begrpt:
        {<<<}
        begin
        getintfile;
        write (' srcindex: ',getintfileint:1);
        getintfile;
        write (' line:', getintfileint: 1);
        end;
        {>>>}
      begexit:
        {<<<}
        begin
        getintfile;
        write (' srcindex: ',getintfileint:1);
        getintfile;
        write (' line:', getintfileint: 1);
        end;
        {>>>}
      gotolab:
        {<<<}
        begin
        getintfile;
        write (' srcindex: ',getintfileint:1);
        getintfile;
        write (' line:', getintfileint: 1);
        getintfile;
        write (' lab:', getintfileint: 1);
        getintfile;
        write(' lev:', getintfileint: 1);
        end;
        {>>>}
      deflab:
        {<<<}
        begin
        getintfile;
        write (' lab:', getintfileint: 1);
        getintfile;
        write (' lev:', getintfileint: 1);
        getintfile;
        write (' nonlocalref:', getintfileint: 1);
        end;
        {>>>}
      endblk:
        {<<<}
        begin
        getintfile;
        write (' sym:', getintfileint: 1);
        end;
        {>>>}
      endfor:
        {<<<}
        begin
        getintfile;
        write (' jump-out-flag:', getintfileint: 1);
        end;
        {>>>}
      endrpt:
        {<<<}
        begin
        getintfile;
        writeln;
        expression;
        end;
        {>>>}
      caselab:
        {<<<}
        begin
        getintfile;
        write (' n:', getintfileint: 1)
        end;
        {>>>}
      caselabrange:
        {<<<}
        begin
        getintfile;
        write (' low:', getintfileint: 1);
        getintfile;
        write (' high:', getintfileint: 1);
        end;
        {>>>}
      blksize:
        {<<<}
        begin
        getintfile;
        write (' size:', getintfileint: 1)
        end;
        {>>>}
      otherwise;
      end;

    getintfile;
    writeln;
  end;
  {>>>}

begin
  new (interSharedPtr);

  writeln ('------- dumpInter - output.inter -------');
  reset (interSharedPtr^.interFile, 'output.inter');

  nextintcode := 0;
  while (not eof (interSharedPtr^.interFile)) and
        (interSharedPtr^.interFile^.block[nextintcode].s <> endall) do
    statement;
  writeln ('endall');

  close (interSharedPtr^.interFile);
  writeln ('------- end of dumpInter -------');
end;
