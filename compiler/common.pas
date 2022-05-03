{ common.pas }
{<<<}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from
  Oregon Software does not apply to user-modified programs.  All copies
  of this program must display this notice and all copyright notices.


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Listing Printer

 Last modified by KRIS on 21-Nov-1990 15:34:50

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}
{>>>}
{$nomain}
%include 'common.def';
%include 'token.def';
%include 'inter.def';
%include 'pseudo.def';

var
  sharedPtr: sharedPtrType;
  tokenSharedPtr: tokenSharedPtrType;
  interSharedPtr: interSharedPtrType;
  pseudoSharedPtr: pseudoSharedPtrType;

{<<<}
procedure initShared;

var
  i: integer;

begin
  new (sharedPtr);
  new (tokenSharedPtr);
  new (interSharedPtr);
  new (pseudoSharedPtr);

  sharedPtr^.current_stmt := 0;
  sharedPtr^.current_line := 0;
  sharedPtr^.fatalflag := false;
  sharedPtr^.lasterror := 0;
  sharedPtr^.sourcelevel := 0;
  sharedPtr^.currentswitch := 1;
  sharedPtr^.genoptmask := 0;

  sharedPtr^.lastlist := 0;

  sharedPtr^.stringfilecount := 0;
  sharedPtr^.stringtablelimit := 0;
  sharedPtr^.stringtabletop := 0;
  sharedPtr^.curstringblock := 1;
  new (sharedPtr^.stringblkptr);
  sharedPtr^.stringblkptrtbl[1] := sharedPtr^.stringblkptr;

  for i := 2 to maxstringblks do
    sharedPtr^.stringblkptrtbl[i] := nil;
  new (sharedPtr^.stringtable);

  sharedPtr^.insertions := 0;
  sharedPtr^.lastswitch := 0;
  sharedPtr^.nextstringfile := 0;
  sharedPtr^.filerememberlist := nil;

  sharedPtr^.shortsection := false;
  sharedPtr^.codesection := oursection; { default code section }
  sharedPtr^.identstring := 0;
  sharedPtr^.identstrlength := 0;
  sharedPtr^.objversion := 0;
  sharedPtr^.objrevision := 0;
  sharedPtr^.codesect_string := 0;    { codesect name }
  sharedPtr^.codesect_strlength := 0; { length of codesect name }
  sharedPtr^.module_string :=    0;   { module name }
  sharedPtr^.module_strlength := 0;   { length of module name }
  sharedPtr^.ownsect_string := 0;     { ownsect name }
  sharedPtr^.ownsect_strlength := 0;  { length of ownsect name }
  sharedPtr^.ident_string := 0;       { ident name}
  sharedPtr^.ident_strlength := 0;    { length of ident name }

  tokenSharedPtr^.tokenCount := 0;
  tokenSharedPtr^.nexttoken.baseline := 0;
end;
{>>>}

{<<<}
function getSharedPtr : sharedPtrType;

begin
  getSharedPtr := sharedPtr;
end;
{>>>}
{<<<}
function getTokenSharedPtr : tokenSharedPtrType;

begin
  getTokenSharedPtr := tokenSharedPtr;
end;
{>>>}
{<<<}
function getInterSharedPtr : interSharedPtrType;

begin
  getInterSharedPtr := interSharedPtr;
end;
{>>>}
{<<<}
function getPseudoSharedPtr : pseudoSharedPtrType;

begin
  getPseudoSharedPtr := pseudoSharedPtr;
end;
{>>>}
