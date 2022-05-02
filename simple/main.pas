program simple (output);
{ simple test program}

%include 'shared.def';
%include 'simple1.def';
%include 'simple2.def';

var
  cmdLine: packed array [1..255] of char;
  cmdLineLen: Integer;
  count: Integer;

  sharedPtr: sharedPtrType;

begin
  P_getcmdline (cmdLine, cmdLineLen);
  Writeln ('simple p2c - cmdLine - ', cmdLine: cmdLineLen);

  for count := 1 to 4 do
    writeln (output, 'simple', count:4);

  sharedPtr := initShared;
  writeln ('- simple ', sharedPtr^.value1, ' ', sharedPtr^.value2);
  sharedPtr^.value1 := 1111;
  sharedPtr^.value2 := 2222;
  simple1;
  simple2;

  { scanFile put test }
  rewrite (sharedPtr^.scanFile, 'scan.tmp');

  {
  block[0] := 16#AA;
  block[1] := 16#BB;
  block[2] := 16#CC;
  block[3] := constsym;
  block[0].byte := 16#AA;
  block[1].byte := 16#BB;
  block[2].byte := 16#CC;
  block[3].byte := constsym;
  block[3].token := constsym;
  }
  with sharedPtr^.scanFile^ do
    begin
    block[0].byte := 16#AA;
    block[1].byte := 16#BB;
    block[2].byte := 16#CC;
    block[3].token := constsym;
    end;
  put (sharedPtr^.scanFile);

  {
  block[0] := 16#11;
  block[1] := 16#22;
  block[2] := 16#33;
  block[3] := constsym;
  block[0].byte := 16#11;
  block[1].byte := 16#22;
  block[2].byte := 16#33;
  block[3].byte := constsym;
  block[3].token := constsym;
  }
  with sharedPtr^.scanFile^ do
    begin
    block[0].byte := 16#11;
    block[1].byte := 16#22;
    block[2].byte := 16#33;
    block[3].token := constsym;
    end;
  put (sharedPtr^.scanFile);

  close (sharedPtr^.scanFile);

  { atFile put test }
  rewrite (sharedPtr^.atFile, 'at.tmp');
  close (sharedPtr^.atFile);

end.
