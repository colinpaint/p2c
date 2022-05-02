{ shared.pas }
{$nomain}
%include 'shared.def';

var
  sharedPtr: sharedPtrType;
  count: Integer;

function getSharedPtr : sharedPtrType;

begin
  getSharedPtr := sharedPtr;
end;


function initShared : sharedPtrType;

begin
  new (sharedPtr);
  sharedPtr^.value1 := 0;
  sharedPtr^.value2 := 0;
  writeln ('- simple initShared ', sharedPtr^.value1, ' ', sharedPtr^.value2);

  for count := 0 to 512 do
    sharedPtr^.scanFile^.block[count].byte := 0;
    {
    sharedPtr^.scanFile^.block[count] := 0;
    sharedPtr^.scanFile^.block[count].byte := 0;
    }

  initShared := sharedPtr;
end;
