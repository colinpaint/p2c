{ simple2.pas }
{$nomain}
%include 'shared.def';
%include 'simple2.def';

procedure simple2;

var
  sharedPtr: sharedPtrType;

begin
  sharedPtr := getSharedPtr;
  writeln ('- simple2 ', sharedPtr^.value1, ' ', sharedPtr^.value2);
end;
