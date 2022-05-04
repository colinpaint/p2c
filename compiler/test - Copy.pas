program test (output);
{ simple test program }
{$norangecheck}
{$nopointercheck}
{$noindexcheck}

  {<<<}
  procedure colin;

  var
    j: 1..20;

  begin
    for j := 4 to 10 do
      Writeln ('colin', j);
  end;
  {>>>}
  {<<<}
  procedure colin1;

  var
    j: 1..20;

  begin
    for j := 4 to 10 do
      Writeln ('colin1', j);
  end;
  {>>>}
  {<<<}
  procedure colin2;

  var
    j: 1..20;

  begin
    for j := 4 to 10 do
      Writeln ('colin2', j);
  end;
  {>>>}
var
  i: 1..20;

begin
  for i := 1 to 20 do
    Writeln ('hello colin', i);
  colin;
end.
