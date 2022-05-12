program test (output);

type
  upperCase = array [char] of char;

var
  i: 0..20;
  j: integer;
  uc: upperCase;
  ch: char;

  procedure simple;

  begin
  Writeln ('hello colin');
  end;

begin
  uc['a'] := 'A';
  uc['A'] := 'A';

  for i := 2 to 20 do
    begin
    j := 3 * i;
    Writeln ('hello colin', i, j, i+j);
    end;

  simple;

  for ch := ' ' to 'Z' do
    Writeln ('convert', uc[ch]);
end.
