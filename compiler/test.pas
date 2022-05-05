program test (output);

  procedure colin;

  var
    i: 1..20;

  begin
  for i := 1 to 20 do
    writeln ('loop',i);
  end;

begin
  Writeln ('hello colin');
  colin;
end.
