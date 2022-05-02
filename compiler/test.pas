program test (output);
{ simple test program }
label 99;
  {<<<}
  procedure colin;

  var
    i,j: integer;

  begin
    i := 2;
    j := i * 2;
    writeln ('colin', i, j);
  end;
  {>>>}
  {<<<}
  procedure colin1;

  var
    i,j: integer;

  begin
    i := 2;
    j := i * 2;
    writeln ('colin', i, j);
  end;
  {>>>}
  {<<<}
  procedure colin2;

  var
    i,j: integer;

  begin
    i := 2;
    j := i * 2;
    writeln ('colin', i, j);
  end;
  {>>>}

begin
  Writeln ('hello colin', 20);
  colin;
  colin1;

  goto 99;
  colin2;
99:
end.
