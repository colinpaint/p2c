{ simple1.pas }
{$nomain}
%include 'shared.def';
%include 'simple1.def';

procedure simple1;

var
  sharedPtr: sharedPtrType;

begin
  sharedPtr := getSharedPtr;
  writeln ('- simple1 ', sharedPtr^.value1, ' ', sharedPtr^.value2);
  sharedPtr^.value1 := 3333;
  sharedPtr^.value2 := 4444;
end;
