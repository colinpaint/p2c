program test (output);

var
  i: 1..20;
  j: 0..255;

begin
  for i := 2 to 10 do
    begin
    j := 3;
    Writeln ('hello colin', i, j, i+j);
    end;
end.
