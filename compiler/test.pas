program test (output);

type
  upperCase = array [char] of char;

var
  i: integer;
  j: integer;
  uc: upperCase;

begin
  uc[64] := 'a';
  for i := 2 to 20 do
    begin
    j := 3 * i;
    Writeln ('hello colin', i, j, i+j);
    end;

  for i := 0 to 255 do
    Writeln ('convert', uc[i]);
end.
