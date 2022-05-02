program test (output);
{ simple test program }

%include 'test1.pas'

var
  count: Integer;
  cmdLine: packed array [1..255] of char;
  cmdLineLen: Integer;

  procedure printtime (pass: packed array [l..h: shortint] of char);

  var
    i, day, month, year, hour, minute, second: Integer;

  begin
    for i := l to h do
      write (pass[i]);

    Timestamp (day, month, year, hour, minute, second);
    writeln (' time ', day:2, ':', month:2, ':', year:4, ' ', hour:2, ':', minute:2, ':', second:2);
  end;

begin
  Writeln ('simple - p2c');

  P_getcmdline (cmdLine, cmdLineLen);
  Writeln ('- cmdLine - ', cmdLine: cmdLineLen);

  printtime ('first pass');

  for count := 1 to 10 do
    writeln (output, 'simple', count:4);

  test1;

  exitst (1);

end.
