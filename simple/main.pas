program simple (output);
{ simple test program}

{
function Len(var s : array [lo..hi:integer] of char) : integer;
procedure Clear(var s : array [lo..hi:integer] of char); external;
procedure ReadString(var f : text; var s : array [lo..hi:integer] of char);
procedure WriteString(var f : text; var s : array [lo..hi:integer] of char);
procedure Concatenate(var d : array [lod..hid:integer] of char; var s : array [los..his:integer] of char);
function Search(var s : array [lo..hi:integer] of char; var s2 : array [lo2..hi2:integer] of char; i : integer) : integer;
procedure Insert(var d : array [lod..hid:integer] of char; var s : array [los..his:integer] of char; i : integer); ;
procedure Assign(var d : array [lo..hi:integer] of char; var s : array [los..his:integer] of char); external;
procedure AssChar(var d : array [lo..hi:integer] of char; c : char);
function Equal(var s1 : array [lo1..hi1:integer] of char; var s2 : array [lo2..hi2:integer] of char) : boolean;
procedure DelString(var s; i, j : integer);
procedure SubString(var d; var s; i, j : integer);
}
const
  cmdlinelength = 132; { max length of a command line}

type
  cmdindex = 1..cmdlinelength; {index into a command line}
  cmdbuffer = packed array [cmdindex] of char; {holds command line}
  strType = packed array [1..20] of char;

var
  cmdlength: cmdindex; { length of command line }
  cmdline: cmdbuffer;  { actual command line }
  first: strType;
  second: strType;
  i: integer;

begin
  cmdlength := 1;
  P_getcmdline (cmdLine, cmdlength);

  Writeln ('hello colin - ', cmdline:cmdlength);

  Assign (first, 'first');
  Assign (second, 'second');
  writeln ('first - ', first, ' second - ', second);

  Concatenate (first, second);
  writeln ('first - ', first, ' second - ', second);
end;
