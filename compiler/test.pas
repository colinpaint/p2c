program test (output);

var
  i: 0..16#FFFF;
  j: integer;

begin
  for i := 2 to 20 do
    begin
    j := 3 * i;
    Writeln ('hello colin', i, j, i+j);
    end;
end.
