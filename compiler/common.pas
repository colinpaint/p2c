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

  with sharedPtr^ do
    {<<<}
    begin
    current_stmt := 0;
    current_line := 0;
    fatalflag := false;
    lasterror := 0;

    sourcelevel := 0;
    for i := 1 to sourcedepth do
      source[i] := nil;

    currentswitch := 1;
    lastswitch := 0;
    genoptmask := 0;

    lastList := 0;
    forceList := false;

    sourceListHead := nil;
    objname := nil;
    macname := nil;
    listname := nil;

    stringfilecount := 0;
    stringtablelimit := 0;
    stringtabletop := 0;
    curstringblock := 1;
    new (stringblkptr);
    stringblkptrtbl[1] := stringblkptr;

    for i := 2 to maxstringblks do
      stringblkptrtbl[i] := nil;
    new (stringtable);

    insertions := 0;
    nextstringfile := 0;
    filerememberlist := nil;

    shortsection := false;
    codesection := oursection; { default code section }
    codesect_string := 0;    { codesect name }
    codesect_strlength := 0; { length of codesect name }
    identstring := 0;
    identstrlength := 0;
    module_string :=    0;   { module name }
    module_strlength := 0;   { length of module name }
    ownsect_string := 0;     { ownsect name }
    ownsect_strlength := 0;  { length of ownsect name }
    ident_string := 0;       { ident name}
    ident_strlength := 0;    { length of ident name }
    objversion := 0;
    objrevision := 0;
    end;
    {>>>}

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
