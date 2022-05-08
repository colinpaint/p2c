program test (output);

type
  upperCase = array [char] of char;

var
  i: integer;
  j: integer;
  uc: upperCase;
  ch: char;

begin
  uc['a'] := 'A';
  uc['A'] := 'A';

  for i := 2 to 20 do
    begin
    j := 3 * i;
    Writeln ('hello colin', i, j, i+j);
    end;


  for ch := ' ' to 'Z' do
    Writeln ('convert', uc[ch]);
end.
