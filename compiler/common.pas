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
procedure add (left, right: integer;  var result: integer; var overflow: boolean);
{ Add two target integers.  If the operation overflows, "overflow" will
  be set, and "result" will be set to the max value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}
begin
  overflow := ((left >= 0) = (right >= 0)) and ((left >= 0) and
              (sharedPtr^.targetmaxint - right < left) or (left < 0) and
              ( - sharedPtr^.targetmaxint - 1 - right > left));
  if overflow then
    if left >= 0 then
      result := sharedPtr^.targetmaxint
    else
      result := - sharedPtr^.targetmaxint - 1 {two's complement}
  else
    result := left + right;
end;
{>>>}
{<<<}
procedure usadd (left, right: integer;  var result: integer; var overflow: boolean);
{ Unsigned add of two target integers.  If the operation overflows,
  "overflow"  is set, and "result" will be set to the max value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}

  var
    usleft, usright: unsignedint;


  begin {usadd}
    usleft := left;
    usright := right;
    overflow := maxusint - usleft < usright;
    if overflow then result := maxusint
    else result := usleft + usright;
  end {usadd} ;
{>>>}

{<<<}
procedure negate (operand: integer; var result: integer; var overflow: boolean);
{ Negate a target integer.  If it overflows, the result will be set to
  the limit at the appropriate sign.
  ****Self hosted version
}


  begin {negate}
    overflow := operand = ( - sharedPtr^.targetmaxint - 1);
    if overflow then result := sharedPtr^.targetmaxint
    else result := - operand;
  end {negate} ;
{>>>}
{<<<}
procedure subtract (left, right: integer; var result: integer; var overflow: boolean);
{ Subtract two target integers.  If the operation overflows, "overflow" will
  be set, and "result" will be set to the max value possible.
  ****Self hosted version
 }


  begin {subtract}
    overflow := ((left >= 0) <> (right >= 0)) and ((left >= 0) and
                (sharedPtr^.targetmaxint + right < left) or (left < 0) and
                ( - sharedPtr^.targetmaxint - 1 + right > left));
    if overflow then
      if left >= 0 then result := sharedPtr^.targetmaxint
      else result := - sharedPtr^.targetmaxint - 1 {two's complement}
    else result := left - right;
  end {subtract} ;
{>>>}
{<<<}
procedure ussubtract (left, right: integer; var result: integer; var overflow: boolean);
{ Unsigned subtract.  The result is required to be unsigned as well
  or overflow will be set.
  ****Self hosted version
}

  var
    usleft, usright: unsignedint;


  begin {ussubtract}
    usleft := left;
    usright := right;
    overflow := usleft < usright;
    if overflow then result := 0
    else result := usleft - usright;
  end {ussubtract} ;
{>>>}

{<<<}
procedure multiply (left, right: integer;  var result: integer; var overflow: boolean);
{ Multiply two target integers.  If the operation overflows, "overflow"
  will be set and "result" will be set to the max signed value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


  begin {multiply}
    if left = -1 then overflow := right = (- sharedPtr^.targetmaxint - 1)
    else if right = -1 then overflow := left = (- sharedPtr^.targetmaxint - 1)
    else if (left <> 0) and (right <> 0) then
      overflow := ((left > 0) = (right > 0)) and ((left > 0) and
                  (sharedPtr^.targetmaxint div left < right) or (left < 0) and
                  (sharedPtr^.targetmaxint div left > right)) or
                  ((left > 0) <> (right > 0)) and ((left > 0) and
                  (( - sharedPtr^.targetmaxint - 1) div left > right) or (left < 0) and
                  (( - sharedPtr^.targetmaxint - 1) div right > left))
    else overflow := false;
    if overflow then
      if (left > 0) = (right > 0) then result := maxint
      else result := - maxint - 1
    else result := left * right;
  end {multiply} ;
{>>>}
{<<<}
procedure usmultiply (left, right: integer; var result: integer; var overflow: boolean);

{ Unsigned equivalent of the multiply routine
  ****self hosted version
}

  var
    usright, usleft: unsignedint;


  begin {usmultiply}
    usright := right;
    usleft := left;
    if (usright = 0) or (usleft = 0) then overflow := false
    else overflow := maxusint div usright < usleft;
    if overflow then result := maxusint
    else result := usleft * usright;
  end {usmultiply} ;
{>>>}

{<<<}
procedure divide (left, right: integer;  var result: integer; var overflow: boolean);
{ Divide two target integers.  If the operation overflows, "overflow"
  will be set and "result" will be set to the max signed value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


  begin {divide}
    overflow := (right = 0) or (left = ( - sharedPtr^.targetmaxint - 1)) and
                (right = - 1);
    if overflow then
      if (left > 0) or (right = - 1) then result := sharedPtr^.targetmaxint
      else result := - sharedPtr^.targetmaxint - 1
    else result := left div right;
  end {divide} ;
{>>>}
{<<<}
procedure usdivide (left, right: integer;  var result: integer; var overflow: boolean);
{ Unsigned version of divide.
  *****16 bit 68K on a PDP11.  The PDP11 doesn't really have an unsigned
  divide instruction, so we have to fake it.
}

  var
    usleft, usright: unsignedint;


  begin {usdivide}
    usleft := left;
    usright := right;
    overflow := usright = 0;
    if overflow then result := maxusint
    else if usright > sharedPtr^.targetmaxint then
      if usright > usleft then result := 0
      else result := 1
    else result := usleft div usright;
  end {usdivide} ;
{>>>}

{<<<}
procedure remainder (left, right: integer;  var result: integer; var overflow: boolean);
{ Take the mod for target integers;  If "right" is zero, the result
  will be zero, and "overflow" will be set.
  ****self hosted version
}


  begin {remainder}
    overflow := right = 0;
    if overflow or (left = ( - sharedPtr^.targetmaxint - 1)) and ((right = - 1) or
       (right = ( - sharedPtr^.targetmaxint - 1))) then
      result := 0
    else if (right = ( - sharedPtr^.targetmaxint - 1)) then result := left
    else if left < 0 then
      if right < 0 then result := ( - left) mod ( - right)
      else result := - ( - left) mod right
    else if right < 0 then result := - (left mod ( - right))
    else result := left mod right;
    { Now that we have the arithmetic remainder, we can "correct"
      it to conform to the Pascal definition of "mod". }
    if true {we want the positive modulus} then
      if result < 0 then result := result + abs(right);
  end {remainder} ;
{>>>}
{<<<}
procedure usremainder (left, right: integer;  var result: integer; var overflow: boolean);
{ unsigned version of remainder
  ****self hosted version
}

  var
    usright, usleft: unsignedint;


  begin {usremainder}
    usright := right;
    usleft := left;
    overflow := usright = 0;
    if overflow then result := 0
    else result := usleft mod usright;
  end {usremainder} ;
{>>>}

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
